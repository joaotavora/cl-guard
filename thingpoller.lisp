(in-package :thingpoller)


;;;
;;;
(defparameter *catch-errors* t)

(defvar *polls*)
(defvar *thingpoller-queue*)

(defun poller-loop (polls &optional queue)
  (let ((*thingpoller-queue* queue)
        (*polls* polls))
    (labels ((poll-thing (thing fn &key (insistent t))
               (push (make-poller thing fn :insistent insistent) *polls*))
             (unpoll-thing (thing)
               (setq *polls* (delete thing *polls* :key #'car))))
      (catch 'stop
        (loop 
          ;; read mail
          (log:debug "Reading mail")
          (loop for (command . args) = (and queue
                                            (queue:dequeue *thingpoller-queue* :wait nil))
                while command
                do (cond ((eq command :stop)
                          (throw 'stop nil))
                         ((eq command :poll-thing)
                          (log:debug "A request to poll something!")
                          (apply #'poll-thing args))
                         ((eq command :unpoll-thing)
                          (apply #'unpoll-thing args))))
          ;; loop over polls
          (log:debug "Looping over ~a polls" (length polls))
          (loop for (thing . fn) in *polls*
                do (restart-case
                       (funcall fn)
                     (remove-poll ()
                       :report (lambda (stream)
                                 (format stream "Stop polling ~a" thing))
                       (unpoll-thing thing))
                     (leave-poll ()
                       :report (lambda (stream)
                                 (format stream "Keep polling ~a" thing)))))
          (log:debug "Sleeping a bit!")
          (sleep 3))))))


(defun make-poller (thing fn &key (insistent t))
  (let ((current-state 'unbound)
        (current-value 'unbound))
    (cons
     thing
     (lambda ()
       (handler-bind
           ((error
              #'(lambda (c)
                  (log:warn "Poller for ~a errored with ~a" thing c)
                  (let ((restart (if insistent
                                     'leave-poll
                                     'remove-poll)))
                    (when (and *catch-errors* (find-restart restart))
                      (invoke-restart restart))))))
         (cond ((exists-p thing)
                (let ((new-state (current-state thing))
                      (new-value))
                  (cond ((or (eq 'unbound current-state)
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
               (t
                (thingpoller-error 'thing-removed-error "~a has been removed" thing))))))))

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


;;; Poller control
;;;
(defun poll (polls)
  (let* ((queue (queue:make-queue)))
    (bt:make-thread
     #'(lambda () (poller-loop polls queue))
     :name "Thingpoller thread")
    queue))



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
        (make-poller (probe-file #p"./thingpoller.lisp")
                     #'(lambda (old new)
                         (log:info "my file has changed from ~a to ~a" old new))
                     nil))

  (setq *test-lambda*
        (make-poller (probe-file #p"/tmp")
                     #'(lambda (old new)
                         (log:info "tmp-file has changed from ~a to ~a" old new))
                     nil))

  (setq *test-lambda*
        (make-poller (probe-file #p"/tmp/shit")
                     #'(lambda (old new)
                         (log:info "shit has changed from ~a to ~a" old new))
                     nil))
  (delete-file "/tmp/shit")

  (defun foo () 'bar)

  (setq *test-lambda*
        (make-poller (make-instance 'symbol-fdefinition :symbol 'foo)
                     #'(lambda (old new)
                         (log:info "foo has changed from ~a to ~a" old new))
                     nil))
  (fmakunbound 'foo)
  
  (funcall *test-lambda*))






