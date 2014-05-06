(defpackage :livereload
  (:use :cl)
  (:export
   ))

(defpackage :guard
  (:use :cl)
  (:export))

(defpackage :thingpoller
  (:use :cl)
  (:export
   #:remove-watch
   #:remove-all-watches
   #:watch-pathname
   #:watch-definition
   #:start
   #:stop
   #:poll-thing
   #:exists-p
   #:current-state
   #:current-value))
