(in-package :guard)

;;; Guards
;;; 
(defclass guard ()
  ((thing :initarg :thing :initform (error "Needs a thing to watch!") :reader thing)
   (listeners :initarg :listeners :initform nil :reader listeners)
   (backend :initarg :backend :initform :thingpoller :reader backend)
   (queue :initform nil)))

(defmethod print-object ((guard guard) stream)
  (print-unreadable-object (guard stream :type t :identity t)
    (format stream "~a (~a listeners)" (thing guard) (length (listeners guard)))))

(defvar *backend-guards*)
(defvar *backend-masters*)
(defvar *guard-thread*)

(defvar *queue*)

(defgeneric register-guard (backend guard)
  ;; before registering a guard, check we're in the the correct thread
  (:method :before (backend guard)
    (declare (ignore backend guard))
    (assert (eq (bt:current-thread) *guard-thread*)))
  ;; by default registering a guard nothing special
  (:method (backend guard))
  ;; after registering a guard with it's backend, push it onto
  ;; *BACKEND-GUARDS* and set its queue slot so it can alert back
  ;; from other threads.
  (:method :after (backend guard)
    (push guard (gethash backend *backend-guards*))
    (setf (slot-value guard 'queue) *queue*)))

(defgeneric changed (guard &rest event-args)
  ;; by default guards notify all its listeners
  (:method (guard &rest event-args)
    (loop for listener in (listeners guard)
          do (apply #'notify-listener listener guard event-args)))
  (:method :before (guard &rest event-args)
    (declare (ignore guard event-args))
    (assert (eq (bt:current-thread) *guard-thread*))))

;; by default, listeners ignore notifications
(defgeneric notify-listener (guard listener &rest event-args)
  (:method (g l &rest args) (declare (ignore g l args))))

(defun alert (guard &rest event-args)
  (queue:enqueue (cons guard event-args)
                 (slot-value guard 'queue)))

(defun backends ()
  (loop for backend being the hash-keys of *backend-guards*
        collect backend))

(defun backend-guards (backend)
  (gethash backend *backend-guards*))

(defun backend-master (backend)
  (gethash backend *backend-masters*))

;; start backend and return an object for controlling it
(defgeneric start-backend (backend))

;; stop backend using the object returned by start backend
(defgeneric stop-backend (backend master))

(defun guard-loop (guards queue)
  (let ((*backend-guards* (make-hash-table :test #'equal))
        (*backend-masters* (make-hash-table :test #'equal))
        (*queue* queue)
        (*guard-thread* (bt:current-thread)))
    (unwind-protect
         (progn
           (loop for guard in guards
                 do (register-guard (backend guard) guard))
           (loop for backend in (backends)
                 do (setf (gethash backend *backend-masters*)
                          (start-backend backend)))
           (loop for message = (queue:dequeue *queue*)
                 while (not (eq 'stop message))
                 for (guard . event-args) = message
                 do (apply #'changed guard event-args)))
      (loop for backend being the hash-keys of *backend-masters*
            for master being the hash-values of *backend-masters*
            do (stop-backend backend master)))))

(defun guard (guards)
  (let* ((queue (queue:make-queue)))
    (bt:make-thread
     #'(lambda () (guard-loop guards queue))
     :name "Guard thread")
    queue))

#+nil
(guard (list (make-instance 'directory-guard :thing *default-pathname-defaults* :listeners '(:coiso :e :tal))))


;;; Bundled guards
;;;
(defclass file-guard (guard)
  ()
  (:default-initargs :backend #+linux :inotify #+mach-o :thingpoller))



(defclass directory-guard (file-guard)
  ())

(defclass definition-guard (guard)
  ((symbol :initarg :symbol :initform (error "Needs a symbol!")))
  (:default-initargs :backend :thingpoller))

(defmethod changed :after ((guard directory-guard) &rest args &key new-files)
  "After notifying listeners as usual, register guards for new files"
  (declare (ignore old-files args))
  (loop for new-file in new-files
        for guard = (make-instance 'file-guard :thing new-file)
        do (register-guard (backend guard) guard)))


;;; thingpoller backend implementation for the bundled guards
;;; 
(defgeneric thingpoller-event-args (w old new)
  (:method ((w file-guard) old new)
    (list :new-mtime new))
  (:method ((w directory-guard) old new)
    (list :new-files
          (if (and (listp old) (listp new))
              (set-difference new old)
              new)))
  (:method (w old new)))

(defun thingpoller-function-for (guard)
  (lambda (old-value new-value)
    (apply #'alert guard
           (thingpoller-event-args guard
                                   old-value
                                   new-value))))


(defmethod register-guard :after ((backend (eql :thingpoller)) g)
  (let ((master (backend-master backend)))
    (when master
      (queue:enqueue `(:poll-thing ,g
                                   ,(thingpoller-function-for g))
                     master))))

(defmethod start-backend ((backend (eql :thingpoller)))
  (thingpoller:poll
   (mapcar #'(lambda (guard)
               (thingpoller:make-poller
                guard
                (thingpoller-function-for guard)))
           (backend-guards :thingpoller))))

(defmethod stop-backend ((backend (eql :thingpoller)) master)
  (queue:enqueue `(:stop) master))

(defmethod thingpoller:exists-p ((guard file-guard))
  (probe-file (thing guard)))

(defmethod thingpoller:current-state ((guard file-guard))
  (file-write-date (thing guard)))

(defmethod thingpoller:current-value ((guard directory-guard))
  (fad:list-directory (thing guard)))


;;; inotify backend
;;;
;; (defvar *inotify-lock* (bt:make-lock))

;; (defmethod lock ((backend (eql :inotify))) *inotify-lock*)

;; (defmethod register-guard ((backend (eql :inotify)) (w))
;;   (inotify:add-watch )
;;   )
