(defpackage :queue
  (:use :cl)
  (:export
   #:queue-empty-p
   #:make-queue
   #:enqueue
   #:dequeue))
(in-package :queue)

(defclass queue ()
  ((lock    :initform (bt:make-lock "queue-lock"))
   (cv      :initform (bt:make-condition-variable :name "queue-cv"))
   (mailbox   :initform (cons nil nil))
   (tail)
   (emptyp  :initform t)))

(defmethod initialize-instance :after ((queue queue) &rest initargs)
  (declare (ignore initargs))
  (with-slots (tail mailbox) queue
    (setf tail mailbox)))

(defun queue-empty-p (queue) (slot-value queue 'emptyp))

(defun make-queue () (make-instance 'queue))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (with-slots (emptyp mailbox) queue
      (if emptyp
          (format stream "no mails")
          (format stream "~a unread mails" (length mailbox))))))

(defgeneric enqueue (object queue)
  (:method (object (queue queue))
    (with-slots (lock) queue
      (bt:with-lock-held (lock)
        (with-slots (cv mailbox tail emptyp) queue
          (let ((mail (cons object nil)))
            (if emptyp
                (setf mailbox mail
                      tail mailbox
                      emptyp nil)
                (setf (cdr tail) mail
                      tail mail))
            (bt:condition-notify cv)
            object))))))

(defgeneric dequeue (queue &key wait)
  (:method ((queue queue) &key (wait t))
    (with-slots (lock) queue
      (bt:with-lock-held (lock)
        (with-slots (cv) queue
          (loop while (and wait (queue-empty-p queue))
                do (bt:condition-wait cv lock)))
        (with-slots (mailbox tail emptyp) queue
          (if emptyp
              (values nil nil)
              (values (prog1 (first mailbox)
                        (if (endp (rest mailbox))
                            (let ((empty (cons nil nil)))
                              (setf mailbox empty
                                    tail empty
                                    emptyp t))
                            (setf mailbox (rest mailbox))))
                      t)))))))

