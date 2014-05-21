(in-package :cl-guard-backend)

(defgeneric make-guard ())
(defgeneric add-watch (pathname guard))
(defgeneric remove-watch (pathname guard))
(defgeneric read-events (guard &key timeout))

(defgeneric event-file (event))
(defgeneric file-created-p (event))
(defgeneric file-deleted-p (event))
(defgeneric file-changed-p (event))
(defgeneric file-attributes-changed-p (event))

(in-package :cl-guard)







