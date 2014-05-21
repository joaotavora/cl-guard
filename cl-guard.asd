;;;; cl-guard.asd

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel)
  (asdf:operate 'asdf:load-op 'cffi))

(asdf:defsystem #:cl-guard
  :name "cl-guard"
  :version "0.1.0"
  :description "File system notifications"
  :author "João Távora <joaotavora@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cffi
               #:cffi-grovel
               #+linux
               #:inotify)
  :components ((:file "package")
               (:file "guard")
               #+windows
               (:module "windows"
                :serial t
                :components ((:file "package")
                             (cffi-grovel:grovel-file "grovel-windows")
                             (:file "guard-windows")))
               #+(or bsd freebsd mach-o)
               (:module "kqueue"
                :serial t
                :components ((:file "package")
                             (cffi-grovel:grovel-file "grovel-kqueue")
                             (:file "guard-kqueue")))
               #+linux
               (:module "inotify"
                :serial t
                :components ((:file "package")
                             (:file "guard-inotify")))))

;; Local Variables:
;; coding: utf-8
;; End:
