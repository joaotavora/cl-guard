(defpackage #:cl-guard-backend
  (:use #:cl)
  (:export
   #:add-watch
   #:remove-watch
   #:make-guard
   #:read-events
   #:file-created-p
   #:file-deleted-p
   #:file-changed-p
   #:file-attributes-changed-p))

(defpackage #:cl-guard
  (:use #:cl)
  (:export))
