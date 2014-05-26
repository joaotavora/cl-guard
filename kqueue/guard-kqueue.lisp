;; Copyright (c) 2012 Chris Howey
;; Copyright (c) 2014 João Távora

(in-package #:cl-guard-kqueue)

(defclass kqueue-tracker ()
 ((fd :accessor kqueue-fd
   :initarg :fd
   :initform 0)
  (hash :accessor kqueue-hash
   :initarg :hash
        :initform nil)))

(defclass kqueue-event ()
  ((watch :initarg :watch)
   (file :initarg :file
         :reader swank-backend::event-file)
   (guard :initarg :guard)))

(defclass file-watch ()
  ((target :initarg :target)
   (fd :accessor watch-fd)))

(defclass directory-watch (file-watch)
  ((file-tuples)))

(defmethod initialize-instance :after ((watch file-watch) &key target)
  (assert (and target (probe-file target)))
  (let ((namestring (namestring target)))
    (cffi:with-foreign-string (c-path namestring)
      (setf (slot-value watch 'fd)
            (cffi:foreign-funcall "open"
                                  :string c-path
                                  :int (logior
                                        ;; TODO: use O_EVTONLY !!!
                                        +open-read-only+
                                        +open-non-block+)
                                  :int)))))

(defmethod initialize-instance :after ((watch directory-watch) &key target)
  (assert (fad:directory-pathname-p target))
  (let ((file-tuples (scan target)))
    (setf (slot-value watch 'file-tuples) file-tuples)))

(defun scan (dir)
  (loop for file in (fad:list-directory dir)
        unless (fad:directory-pathname-p file)
          collect (list file
                        (file-write-date file))))

(cffi:defbitfield kevent-flags
  (:ev-add       #.+kqueue-flag-add+)
  (:ev-clear     #.+kqueue-flag-clear+)
  (:ev-delete    #.+kqueue-flag-delete+))

(cffi:defbitfield kevent-filter
  (:evfilt-vnode #.+kqueue-fflag-vnode+))

(cffi:defbitfield kevent-fflags
 (:note-delete  #.+kqueue-note-delete+)
 (:note-write   #.+kqueue-note-write+)
 (:note-extend  #.+kqueue-note-extend+)
 (:note-attrib  #.+kqueue-note-attrib+)
 (:note-link    #.+kqueue-note-link+)
 (:note-rename  #.+kqueue-note-rename+)
 (:note-revoke  #.+kqueue-note-revoke+))

(cffi:defcfun kqueue :int
  "Creates a new kernel event queue.")

(cffi:defcfun kevent :int
  "Register and return events."
  (kq            :int)
  (changelist    :pointer)
  (nchanges      :int)
  (eventlist     :pointer)
  (nevents       :int)
  (timeout       :pointer))

(cffi:defcstruct struct-kevent
  "kevent structure"
  (ident      :unsigned-long)
  (filter     :short)
  (flags      :unsigned-short)
  (fflags     :unsigned-int)
  (data       :long)
  (udata      :pointer))

(cffi:defcstruct struct-timespec
  "timespec"
  (tv-sec :long)
  (tv-nsec :long))

(cffi:defcvar errno :int)

(cffi:defcfun strerror :string (errno :int))

(defparameter *flags*
  (loop for name in
        '(+kqueue-flag-add+
          +kqueue-flag-clear+
          +kqueue-flag-delete+
          +kqueue-fflag-vnode+
          +kqueue-note-delete+
          +kqueue-note-write+
          +kqueue-note-extend+
          +kqueue-note-attrib+
          +kqueue-note-link+
          +kqueue-note-rename+
          +kqueue-note-revoke+)
        collect (cons (symbol-value name) name)))

(defun event-names (fflags)
  (loop for (value . name) in
        '#.(loop for name in
                 '(+kqueue-note-delete+
                   +kqueue-note-write+
                   +kqueue-note-extend+
                   +kqueue-note-attrib+
                   +kqueue-note-link+
                   +kqueue-note-rename+
                   +kqueue-note-revoke+)
                 collect (cons (symbol-value name) name))
        when (logtest value fflags)
          collect name))

(defmethod cl-guard-backend:make-guard ()
  (make-instance 'kqueue-tracker :fd (kqueue)
                                 :hash (make-hash-table :test #'equal)))

(defun cl-guard-backend:destroy-guard (kq)
  (let ((hash (kqueue-hash kq)))
    (maphash #'(lambda (path fd)
                 (cffi:foreign-funcall "close" :int fd :int)
                 (remhash path hash)) hash)
    (cffi:foreign-funcall "close" :int (kqueue-fd kq) :int)))

(defmethod cl-guard-backend:add-watch (pathname (guard kqueue-tracker))
  (let* ((watch (or (gethash pathname (kqueue-hash guard))
                    (setf (gethash pathname (kqueue-hash guard))
                          (make-instance
                           (if (fad:directory-pathname-p pathname)
                               'directory-watch
                               'file-watch)
                           :target pathname)))))
    (cffi:with-foreign-object (kev '(:struct struct-kevent))
      (cffi:with-foreign-slots ((ident
                                 filter
                                 flags
                                 fflags
                                 udata) kev (:struct struct-kevent))
        (setq ident (watch-fd watch)
              filter (cffi:foreign-bitfield-value 'kevent-filter
                                                  '(:evfilt-vnode))
              flags  (cffi:foreign-bitfield-value 'kevent-flags
                                                  '(:ev-add :ev-clear))
              fflags (cffi:foreign-bitfield-value 'kevent-fflags
                                                  '(:note-delete
                                                    :note-write
                                                    :note-extend
                                                    :note-attrib
                                                    :note-link
                                                    :note-rename
                                                    :note-revoke))
              ;; BROKEN
              udata (cffi:convert-to-foreign watch :pointer)))
      (kevent
       (kqueue-fd guard) kev 1 (cffi:null-pointer) 0 (cffi:null-pointer)))))

(defmethod cl-guard-backend:remove-watch (pathname (guard kqueue-tracker))
  (let ((watch (gethash pathname (kqueue-hash guard))))
    (when watch
      (cffi:with-foreign-object (kev '(:struct struct-kevent))
        (cffi:with-foreign-slots ((ident
                                   filter
                                   flags
                                   fflags
                                   udata) kev (:struct struct-kevent))
          (setf ident (watch-fd watch)
                filter (cffi:foreign-bitfield-value 'kevent-filter
                                                    '(:evfilt-vnode))
                flags  (cffi:foreign-bitfield-value 'kevent-flags
                                                    '(:ev-delete))
                fflags (cffi:foreign-bitfield-value 'kevent-fflags
                                                    '(:note-delete
                                                      :note-write
                                                      :note-extend
                                                      :note-attrib
                                                      :note-link
                                                      :note-rename
                                                      :note-revoke))
                udata (cffi:convert-to-foreign watch :pointer)))
        (remhash pathname (kqueue-hash guard))
        (cffi:foreign-funcall "close" :int (watch-fd watch) :int)
        (kevent
         (kqueue-fd guard) kev 1 (cffi:null-pointer) 0 (cffi:null-pointer))))))

(defmethod cl-guard-backend:read-events ((guard kqueue-tracker) &key (timeout 0))
  (cffi:with-foreign-objects ((kev '(:struct struct-kevent))
                              (timespec '(:struct struct-timespec)))
    (cffi:with-foreign-slots ((tv-sec tv-nsec)
                              timespec (:struct struct-timespec))
      (multiple-value-bind (seconds miliseconds)
          (truncate timeout 1000)
        (setf tv-sec seconds
              tv-nsec (* miliseconds 1000)))
      (let* ((retval (kevent (kqueue-fd guard) (cffi:null-pointer) 0 kev 1 timespec)))
        (cffi:with-foreign-slots ((fflags udata) kev (:struct struct-kevent))
          (cond ((= retval 0)
                 nil)
                ((and (= retval 1)
                      (logand fflags +kqueue-note-write+))
                 (setf tv-sec 0 tv-nsec 0)
                 ;; (loop for ret2 = (kevent (kqueue-fd guard) (cffi:null-pointer) 0 kev 1 timespec)
                 ;;       while (> ret2 0)
                 ;;       finally (unless (eq ret2 0)
                 ;;                 )
                 ;;       )
                 (let ((watch (cffi:convert-from-foreign udata :pointer)))
                   (break "got this ~a" watch)
                   (make-instance 'kqueue-event :guard guard)

                   )
                 )
                (t
                 (error "Unexpected error: ~a" (strerror *errno*)))))))))



