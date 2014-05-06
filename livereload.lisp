(defpackage :livereload (:use :cl))
(in-package :livereload)


;;; Hunchensocket/Hunchentoot server.
;;; 
;;; Serves /livereload.js if it exists in *LIVERELOAD-ROOT*

(defvar *this-file* #.(or *compile-file-truename* *load-truename*))

(defvar *livereload-root* (make-pathname :defaults *this-file*
                                         :name nil
                                         :type nil))

(defclass livereload-acceptor (hunchensocket:websocket-acceptor
                               hunchentoot:easy-acceptor)
  ())

(defvar *livereload-server* (make-instance 'livereload-acceptor
                                           :port 35729
                                           :document-root *livereload-root*))


;;; Setup resource and client classes to serve websockets on
;;; /livereload
(defclass livereload-resource (hunchensocket:websocket-resource)
  ()
  (:default-initargs :client-class 'browser-tab))

(defclass browser-tab (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this browser-tab!"))
   (url  :initform nil :accessor browser-tab-url)))

(defvar *livereload-resource* (make-instance 'livereload-resource))

(defun connected-browser-tabs () (hunchensocket:clients *livereload-resource*))

(defun find-livereload (request)
  (and (string-equal (hunchentoot:script-name request) "/livereload")
       *livereload*))

(pushnew 'find-livereload hunchensocket:*websocket-dispatch-table*)


;;; Machinery
;;;
(define-condition livereload-error (simple-error)
  ()
  (:documentation "Superclass for all errors related to Livereload."))

(defun livereload-error (format-control &rest format-arguments)
  (error 'livereload-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition livereload-warning (simple-warning)
  ()
  (:documentation "Superclass for all warnings related to Livereload."))

(defun livereload-warning (format-control &rest format-arguments)
  (warn 'livereload-warning
        :format-control format-control
        :format-arguments format-arguments))



;;; Speak the actual protocol
;;;
(defmethod hunchensocket:text-message-received ((resource livereload-resource)
                                                (browser-tab browser-tab)
                                                message)
  (json:decode-json-from-string "{\"command\":\"hello\",\"protocols\":[\"http://livereload.com/protocols/connection-check-1\"]}")
  (let* ((decoded (json:decode-json-from-string message))
         (command (cdr (assoc :command decoded)))
         (protocols (cdr (assoc :protocols decoded))))
    (declare (ignore protocols))
    (cond ((string= command "hello")
           (hunchensocket:send-text-message
            browser-tab
            (json:encode-json-alist-to-string
             '((:command . "hello")
               (:protocols "http://livereload.com/protocols/official-7")))))
          (t
           (livereload-warning "Unknown message: ~a" decoded)))))

(defun notify-tab (tab path)
  (hunchensocket:send-text-message
   tab
   (json:encode-json-plist-to-string `(:command "reload"
                                       :path ,path))))


;;; Example hunchentoot routes running on the very same server
;;; 
(hunchentoot:define-easy-handler (example :uri "/example") ()
  (who:with-html-output-to-string (s nil :prologue "<!doctype html>")
    (:html
     (:head
      (:title "Livereload Fornix!"))
     (:body
      (:div :class "coiso" :style "background-color: peachpuff"
            (:p "E esta caralho")
            (:p "Hmmm...")
            (:p "Ah pois e bebe!"))
      (:div :class "coiso" :style "background-color: lightslategrey"
            (:p "E esta caralho")
            (:p "Hmmm...")
            (:p "Ah pois e bebe!"))))))











;;; Main blocking loop
;;; 



(livereload "http://localhost:35729"
            '(#p"./example/example.css"
              #p"./example/subdir"
              '(example "/example")))




