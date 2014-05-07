(in-package :guard)

;;; Guards and watches
;;;
(defclass guard ()
  ((watches :initform nil :reader watches)
   (listeners :initarg :listeners :initform nil :reader listeners)
   (backends :initform nil :accessor backends)
   (queue :initform (queue:make-queue) :reader queue)))

(defclass watch ()
  ((target :initarg :target :initform (error "watch needs a target")
           :reader target)
   (listeners :initarg :listeners :initform nil :reader listeners)
   (backend-type :initarg :backend-type :initform (error "needs a backend type")
                 :reader backend-type)
   (guard :initarg :guard :reader guard)
   (backend :reader backend)))

(defgeneric backend-initargs (type guard)
  (:method (type guard)
    "By default, backends are created without initargs."
    (declare (ignore type guard))))
  
(defmethod print-object ((guard guard) stream)
  (print-unreadable-object (guard stream :type t)
    (with-slots (listeners watches backends) guard
      (princ "(" stream)
      (when listeners
        (format stream "~a listeners, " (length listeners)))
      (format stream "~a watches, " (length watches))
      (format stream "~a backends" (length backends)))
      (princ ")" stream)))

(defmethod print-object ((watch watch) stream)
  (print-unreadable-object (watch stream :type t)
    (with-slots (thing guard listeners backend) watch
      (format stream "watching ~a" (target watch))
      (princ " (" stream)
      (when listeners
        (format stream "~a listeners, " (length listeners)))
      (princ ")" stream))))

(defmethod initialize-instance :after ((g guard) &key watches &allow-other-keys)
  (loop for watch in watches
        do (add-watch watch g)))

(defun add-watch (watch guard)
  "Before adding a watch to the guard, check we're in the the correct thread.
Also ensure GUARD has a instantiated backend that will hold this
watch. Register the WATCH with this backend."
  (let ((backend
          (or (find (backend-type watch)
                    (backends guard)
                    :key #'type-of)
              (apply #'make-instance (backend-type watch)
                     (backend-initargs (backend-type watch) guard)))))
    (setf (slot-value watch 'guard) guard)
    (register-watch watch backend)
    (pushnew backend (backends guard))
    (push watch (slot-value guard 'watches))
    (setf (slot-value watch 'listeners)
          (remove-duplicates (append (listeners watch)
                                     (listeners guard))))
    watch))

(defgeneric register-watch (watch backend)
  (:method :after (watch backend)
    (setf (slot-value watch 'backend) backend)))

(defmethod deregister-watch (watch backend))

(defgeneric notify-listener (watch listener &rest event-args)
  (:method (g l &rest args)
    "By default, listeners ignore notifications."
    (declare (ignore g l args))))

(defgeneric changed (watch backend &rest event-args)
  (:method :before (watch backend &rest event-args)
    "Ensure this method is called in the correct thread"
    (declare (ignore watch backend event-args))
    (assert (eq (bt:current-thread) *guard-thread*)))
  (:method (watch backend &rest event-args)
    (declare (ignore backend))
    "By default watchess notify all its listeners"
    (loop for listener in (listeners watch)
          do (apply #'notify-listener listener watch event-args))))

(defun alert (watch &rest event-args)
  (queue:enqueue (cons watch event-args)
                 (slot-value (guard watch) 'queue)))

(defgeneric start-backend (backend)
  (:documentation "Start backend BACKEND"))

(defgeneric stop-backend (backend)
  (:documentation "Stop the running backend BACKEND"))

(defun guard-loop (guard thread)
  (let ((*guard-thread* thread))
    (unwind-protect
         (progn
           (mapc #'start-backend (backends guard))
           (loop for message = (queue:dequeue (queue guard))
                 while (not (eq 'stop message))
                 for (watch . event-args) = message
                 do (apply #'changed watch (backend watch) event-args)))
      (mapc #'stop-backend (backends guard))
      (mapc (lambda (watch)
              (deregister-watch watch (backend watch)))
            (watches guard)))))


(defun make-guard (watches &rest listeners)
  (make-instance 'guard :watches watches :listeners listeners))

(defun start-guard (guard)
  (bt:make-thread
   #'(lambda () (guard-loop guard (bt:current-thread)))
   :name "Guard thread"))

#+nil
(make-guard (list (make-instance 'directory-watch
                                 :target *default-pathname-defaults* ))
            :ola :ole)


;;; Bundled watchs
;;;
(defclass file-watch (watch)
  ()
  (:default-initargs :backend-type #+linux :inotify 'thingpoller:poller))

(defmethod initialize-instance :after ((watch file-watch) &key)
  (assert (pathnamep (target watch)) nil
          "FILE-WATCH can only existing files"))

(defclass directory-watch (file-watch)
  ())

(defmethod initialize-instance :after ((watch directory-watch) &key)
  (assert (fad:directory-pathname-p (target watch)) nil
          "DIRECTORY-WATCH can only watch directories"))

(defclass definition-watch (watch)
  ()
  (:default-initargs :backend-type 'thingpoller::poller))

(defmethod initialize-instance :after ((watch definition-watch) &key)
  (assert (symbolp (target watch)) nil
          "DEFINITION-WATCH can only watch symbols"))


;;; thingpoller backend implementation for the bundled watch classes
;;;
(defclass subwatch (file-watch)
  ((parent-watch :initarg :parent-watch :initform (error "need a parent watch")
                 :reader parent-watch))
  (:default-initargs :backend-type 'thingpoller:poller))

(defmethod changed ((watch subwatch) (backend thingpoller:poller)
                    &key deleted-p)
  "Forward the change event to the PARENT-WATCH of WATCH."
  (changed (parent-watch watch) backend
           (if deleted-p :deleted-files :changed-files)
           (list (target watch))))

(defun thingpoller-register-subwatch (file watch backend)
  (register-watch (make-instance 'subwatch
                                 :parent-watch watch
                                 :target file
                                 :listeners (list watch)
                                 :guard (guard watch))
                  backend))

(defmethod register-watch :after ((watch directory-watch) (backend thingpoller:poller))
  (loop for file in (fad:list-directory (target watch))
        unless (fad:directory-pathname-p file)
          do (thingpoller-register-subwatch file watch backend)))

(defmethod changed ((watch directory-watch) (backend thingpoller:poller)
                    &rest args &key new-files deleted-files)
  "After notifying listeners as usual, register watches for new files"
  (declare (ignore args))
  (unless deleted-files
    (call-next-method)
    (loop for new-file in new-files
        do (thingpoller-register-subwatch new-file watch backend))0))

(defgeneric thingpoller-event-args (w old new)
  (:method ((w file-watch) old new)
    (if (eq new 'thingpoller:inexistent)
        (list :deleted-p t)
        (list :new-mtime new)))
  (:method ((w directory-watch) old new)
    (list :new-files
          (if (and (listp new) (listp old))
              (set-difference new old :test #'equal)
              new)))
  (:method (w old new)))

(defun thingpoller-function-for (watch)
  (lambda (old-value new-value)
    (apply #'alert watch
           (thingpoller-event-args watch
                                   old-value
                                   new-value))))

(defmethod register-watch (watch (backend thingpoller:poller))
  (push (make-instance 'thingpoller:poll
                       :thing watch
                       :fn (thingpoller-function-for watch))
        (thingpoller:polls backend)))

(defmethod deregister-watch (watch (backend thingpoller:poller))
  "Thingpoller automatically cleans up, no need."
  (declare (ignore watch))
  nil)

(defmethod start-backend ((backend thingpoller:poller))
  (bt:make-thread
     #'(lambda () (thingpoller:start-polling backend))
     :name "Thingpoller thread")
  (let* ((poller (thingpoller:make-poller)))
    
    poller))

(defmethod stop-backend ((backend thingpoller:poller))
  (thingpoller:stop-polling backend))

(defmethod thingpoller:exists-p ((watch file-watch))
  (probe-file (target watch)))

(defmethod thingpoller:current-state ((watch file-watch))
  (file-write-date (target watch)))

(defmethod thingpoller:current-value ((watch directory-watch))
  (fad:list-directory (target watch)))


;;; inotify backend
;;;
;; (defvar *inotify-lock* (bt:make-lock))

;; (defmethod lock ((backend (eql :inotify))) *inotify-lock*)

;; (defmethod register-watch ((backend (eql :inotify)) (w))
;;   (inotify:add-watch )
;;   )
