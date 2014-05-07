(in-package :thingpoller)


;;;
;;;
(defparameter *catch-errors* t)

(defclass poller ()
  ((polls :initarg :polls :initform nil :accessor polls)
   (runningp :initform nil :accessor runningp)
   (stop-flag :initform nil :accessor stop-flag)
   (lock :initform (bt:make-lock) :reader lock)))

(defclass poll ()
  ((thing :initarg :thing :initform (error "Need a thing!") :reader thing)
   (fn :initarg :fn :initform (error "Need a function!"))
   (wrapper)))

(defmethod print-object ((p poll) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "polling ~a" (thing p))))

(defmethod initialize-instance :after ((poll poll) &key thing fn)
  (setf (slot-value poll 'wrapper)
        (make-poll-function thing fn)))

(defmethod (setf polls) :around (new-value (p poller))
  (declare (ignore new-value))
  (bt:with-lock-held ((slot-value p 'lock))
    (call-next-method)))

(defun start-polling (poller)
  (when (runningp poller)
    (error "Poller ~a already started!" poller))
  (setf (runningp poller) t
        (stop-flag poller) nil)
  (unwind-protect
       (loop until (stop-flag poller)
             do
                ;; loop over polls
                (log:debug "Looping over ~a polls" (length (polls poller)))
                (loop for poll in (polls poller)
                      for thing = (thing poll)
                      for wrapper = (slot-value poll 'wrapper)
                      do (restart-case
                             (funcall wrapper)
                           (remove-poll ()
                             :report (lambda (stream)
                                       (format stream "Stop polling ~a" thing))
                             (setf (polls poller) (delete thing (polls poller)
                                                          :key #'thing)))
                           (leave-poll ()
                             :report (lambda (stream)
                                       (format stream "Keep polling ~a" thing)))))
                (log:debug "Sleeping a bit!")
                (sleep 3))
    (setf (runningp poller) nil
          (stop-flag poller) nil)))

(defun stop-polling (poller)
  (setf (stop-flag poller) t)
  (loop while (runningp poller)
        for i from 10 downto 0
        when (zerop i)
          do (log:warn "Can't stop poller ~a" poller)
             (return)
        do (log:info "Waiting ~a seconds for ~a poller to stop..." i poller)
           (sleep 1)))

(defun make-poll-function (thing fn)
  (let ((current-state (if (exists-p thing)
                           (current-state thing)
                           'inexistent))
        (current-value (if (exists-p thing)
                           (current-value thing)
                           'unbound)))
    (lambda ()
      (handler-bind
          ((error
             #'(lambda (c)
                 (let ((restart 'remove-poll))
                   (when (and *catch-errors* (find-restart restart))
                     (log:warn "Poller for ~a errored with ~a. Removing." thing c)
                     (invoke-restart restart))))))
        (cond ((exists-p thing)
               (let ((new-state (current-state thing))
                     (new-value))
                 (cond ((or (eq 'inexistent current-state)
                            (not (funcall (test thing) current-state new-state)))
                        (log:debug "Thing ~a changed from state ~a to ~a"
                                   thing current-state new-state)
                        (setq new-value (current-value thing))
                        (funcall fn
                                 (if (eq 'unbound new-value) current-state current-value)
                                 (if (eq 'unbound new-value) new-state new-value))
                        (setq current-state new-state
                              current-value new-value))
                       (t
                        (log:debug "Thing ~a unchanged in state ~a"
                                   thing current-state)))))
              ((not (eq current-state 'inexistent))
               (funcall fn
                        (if (eq 'unbound current-value) current-state current-value)
                        'inexistent)
               (thingpoller-error 'thing-removed-error "~a has been removed" thing)))))))

(define-condition thingpoller-error (simple-error)
  ())

(define-condition thing-removed-error (thingpoller-error)
  ())

(defun thingpoller-error (thing format-control &rest format-arguments)
  (error thing :format-control format-control :format-arguments format-arguments))


;;; API
;;; 
(defgeneric exists-p (thing))
(defgeneric current-state (thing))
(defgeneric current-value (thing) ;; optional
  (:method (thing) 'unbound))
(defgeneric test (thing)
  (:method (thing) #'eql))
(defun make-poller (&rest polls)
  (make-instance 'poller :polls polls))


;;; Polling files and directories
;;;
(defmethod exists-p ((path pathname))
  (probe-file path))

(defmethod current-state ((path pathname))
  (file-write-date path))

(defmethod current-value ((path pathname))
  (if (fad:directory-pathname-p path)
      (fad:list-directory path)
      (call-next-method)))

;;; Polling definitions
;;
(defclass symbol-fdefinition ()
  ((symbol :initarg :symbol :initform (error "Requires SYMBOL"))))

(defmethod exists-p ((definition symbol-fdefinition))
  (fboundp (slot-value definition 'symbol)))

(defmethod current-state ((definition symbol-fdefinition))
  (symbol-function (slot-value definition 'symbol)))




;;; Crappy interactive tests
#+nil
(progn
  (defvar *test-lambda*)
  (setq *test-lambda*
        (make-poll (probe-file #p"./thingpoller.lisp")
                     #'(lambda (old new)
                         (log:info "my file has changed from ~a to ~a" old new))
                     nil))

  (setq *test-lambda*
        (make-poll (probe-file #p"/tmp")
                     #'(lambda (old new)
                         (log:info "tmp-file has changed from ~a to ~a" old new))
                     nil))

  (setq *test-lambda*
        (make-poll (probe-file #p"/tmp/shit")
                     #'(lambda (old new)
                         (log:info "shit has changed from ~a to ~a" old new))
                     nil))
  (delete-file "/tmp/shit")

  (defun foo () 'bar)

  (setq *test-lambda*
        (make-poll (make-instance 'symbol-fdefinition :symbol 'foo)
                     #'(lambda (old new)
                         (log:info "foo has changed from ~a to ~a" old new))
                     nil))
  (fmakunbound 'foo)
  
  (funcall *test-lambda*))






