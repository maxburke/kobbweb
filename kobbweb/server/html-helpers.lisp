;;;; Contains helpers for handling html page generation (like generating headers and footers)

(in-package :kobbweb)

(defmacro with-posted-json ((json) &body body)
 (let ((raw-json-string (gensym)))
  `(let* ((,raw-json-string (octets-to-string (raw-post-data :request *request*) :external-format :utf8))
          (,json (handler-case (json:decode-json-from-string ,raw-json-string)
                      (end-of-file () (server-log (format nil "unable to parse json request ~a" ,raw-json-string))
                                      nil))))
    ,@body
   )
 )
)

