(in-package :cl-guard-windows)

;;; ffi

(defconstant +invalid-handle-value+          -1)
(defconstant +true+                          1)
(defconstant +false+                         0)
(defconstant +wait_abandoned+                #x00000080)
(defconstant +wait-object-0+                 #x00000000)
(defconstant +wait-timeout+                  #x00000102)
(defconstant +wait-failed+                   #xFFFFFFFF)
(defconstant +infinite+                      #xFFFFFFFF)
(defconstant +file-notify-change-file-name+  #x00000001)
(defconstant +file-notify-change-dir-name+   #x00000002)
(defconstant +file-notify-change-attributes+ #x00000004)
(defconstant +file-notify-change-size+       #x00000008)
(defconstant +file-notify-change-last-write+ #x00000010)
(defconstant +file-notify-change-security+   #x00000100)

(cffi:defctype bool :int)
(cffi:defctype dword :unsigned-long)
(cffi:defctype handle :pointer)

(cffi:defcfun (wait-for-single-object "WaitForSingleObject")
    handle
  (handle handle)
  (milliseconds dword))

(cffi:defcfun (wait-for-single-object "WaitForSingleObject") handle
  (handle handle)
  (milliseconds dword))

;; DWORD WINAPI WaitForMultipleObjects(
;;   _In_  DWORD nCount,
;;   _In_  const HANDLE *lpHandles,
;;   _In_  BOOL bWaitAll,
;;   _In_  DWORD dwMilliseconds
;; );

(cffi:defcfun (wait-for-multiple-objects "WaitForMultipleObjects") dword
  (n-count      dword)
  (handles      :pointer)
  (b-wait-all   bool)
  (milliseconds dword))

(cffi:defcfun (find-first-change-notification
		   "FindFirstChangeNotificationA") :uintptr
  (path-name :string)
  (watch-subtree bool)
  (notify-filter dword))

(cffi:defcfun (find-next-change-notification
               "FindNextChangeNotification") bool
  (handle handle))

(cffi:defcfun (find-close-change-notification
               "FindCloseChangeNotification") bool
  (handle handle))

;; Read
;; http://lists.gnu.org/archive/html/emacs-diffs/2012-12/msg00163.html
;; and
;; http://qualapps.blogspot.pt/2010/05/understanding-readdirectorychangesw_19.html
;; for why I chose this simplified approach

(defclass windows-guard ()
  ((watches)))

(defclass directory-watch ()
  ((handle :initarg :handle :reader watch-handle)
   (pathname :initarg :pathname)))

(defmethod cl-guard-backend:add-watch (pathname (guard windows-guard))
  (let ((handle (find-first-change-notification (namestring pathname)
                                                +false+ ;; don't watch subtrees
                                                (logior +file-notify-change-file-name+
                                                        +file-notify-change-dir-name+
                                                        +file-notify-change-attributes+
                                                        +file-notify-change-last-write+))))
    (with-slots (watches) guard
      (if (and handle
               (not (= handle +invalid-handle-value+)))
          (push (make-instance 'directory-watch
                               :handle handle
                               :pathname pathname)
                watches)
          (error "cannot watch ~a, handle ~a returned invalid " pathname handle)))))

(defmethod cl-guard-backend:make-guard () (make-instance 'windows-guard))

(defmethod cl-guard-backend:read-events ((guard windows-guard) &key timeout)
  (with-slots (watches) guard
    (let* ((handle-vector (coerce
                           (loop for watch in watches
                                 collect (watch-handle watch))
                           'vector))
           (retval (wait-for-multiple-objects (length handle-vector)
                                              handle-vector
                                              +false+ ;; any change will do
                                              (or timeout
                                                  +infinite+))))
      (case retval
        (+wait-failed+
         (error "waiting for events on ~a failed with ~a" guard retval))
        (+wait-timeout+
         nil)
        (otherwise
         (let* ((idx (- retval
                        +wait-object-0+))
                (handle (aref handle-vector idx))
                (watch (find handle watches :key #'watch-handle)))
           (assert watch)
           ;; re-arm the watch
           (find-next-change-notification handle)))))))






