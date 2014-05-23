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

(cffi:defcfun ("kqueue" c-kqueue) :int
  "Creates a new kernel event queue.")

(cffi:defcfun ("kevent" c-kevent) :int
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

(defmethod cl-guard-backend:make-guard ()
  (make-instance 'kqueue-tracker :fd (c-kqueue)
                                 :hash (make-hash-table :test #'equal)))

(defun cl-guard-backend:destroy-guard (kq)
  (let ((hash (kqueue-hash kq)))
    (maphash #'(lambda (path fd)
                 (cffi:foreign-funcall "close" :int fd :int)
                 (remhash path hash)) hash)
    (cffi:foreign-funcall "close" :int (kqueue-fd kq) :int)))

(defmethod cl-guard-backend:add-watch (path (kq kqueue-tracker))
  (assert (probe-file path))
  (let ((fd (or (gethash path (kqueue-hash kq))
                (cffi:with-foreign-string (c-path path)
                  (cffi:foreign-funcall "open"
                                        :string c-path
                                        :int (logior
                                              +open-read-only+
                                              +open-non-block+)
                                        :int)))))
    (setf (gethash path (kqueue-hash kq)) fd)
    (cffi:with-foreign-object (kev '(:struct struct-kevent))
      (cffi:with-foreign-slots ((ident
                                 filter
                                 flags
                                 fflags
                                 udata) kev (:struct struct-kevent))
        (setq ident fd
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
              udata (cffi:convert-to-foreign path :string)))
      (c-kevent
       (kqueue-fd kq) kev 1 (cffi:null-pointer) 0 (cffi:null-pointer)))))

(defmethod cl-guard-backend:remove-watch (path (kq kqueue-tracker))
  (let ((fd (gethash path (kqueue-hash kq))))
    (when fd
      (cffi:with-foreign-object (kev '(:struct struct-kevent))
        (cffi:with-foreign-slots ((ident
                                   filter
                                   flags
                                   fflags
                                   udata) kev (:struct struct-kevent))
          (setf ident (gethash path (kqueue-hash kq))
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
                udata (cffi:convert-to-foreign path :string)))
        (c-kevent
         (kqueue-fd kq) kev 1 (cffi:null-pointer) 0 (cffi:null-pointer)))
      (remhash path (kqueue-hash kq))
      (cffi:foreign-funcall "close" :int fd :int))))

(defun get-event (kq-fd)
  ;; TODO implement timeouts
  (cffi:with-foreign-objects ((kev '(:struct struct-kevent))
                              (timespec '(:struct struct-timespec)))
    (cffi:with-foreign-slots ((tv-sec tv-nsec)
                              timespec (:struct struct-timespec))
      (setf tv-sec 0
            tv-nsec 0))
    (when (= (c-kevent kq-fd (cffi:null-pointer) 0 kev 1 timespec) 1)
      (cffi:convert-from-foreign
       (cffi:foreign-slot-value kev '(:struct struct-kevent) 'udata) :string))))

(defmethod cl-guard-backend:read-events ((kq kqueue-tracker) &key timeout)
  (declare (ignore timeout))
  (loop as event = (get-event (kqueue-fd kq))
        while event
        collect event))
