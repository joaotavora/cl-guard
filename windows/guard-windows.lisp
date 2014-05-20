(in-package :cl-guard-windows)

(defclass windows-guard ()
  ())

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

(defmethod cl-guard-backend:add-watch (pathname (guard windows-guard))
  (find-first-change-notification (namestring pathname)
                                  +false+
                                  (logior +file-notify-change-file-name+)))


(defmethod cl-guard-backend:make-guard () (make-instance 'windows-guard))
