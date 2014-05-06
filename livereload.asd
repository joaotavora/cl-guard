(asdf:defsystem :thingpoller
  :depends-on (#:bordeaux-threads
               #:cl-fad
               #:log4cl)
  :serial t
  :components ((:file "package")
               (:file "thingpoller")))

(asdf:defsystem :guard
  :depends-on (#:bordeaux-threads
               #:cl-fad
               #:log4cl)
  :serial t
  :components ((:file "package")
               (:file "guard")))

(asdf:defsystem :livereload
  :depends-on (#:hunchensocket
               #:bordeaux-threads
               #:cl-who
               #:cl-json
               #:thingpoller)
  :serial t
  :components ((:file "package")
               (:file "livereload")))

