(in-package :kobbweb)

; GET /data/<ref> returns the raw data at that endpoint.
(defun data-handle-get (ref)
 (if (null ref)
  (setf (return-code*) +http-not-found+)
  (let ((data (cas-load (hex-string-to-byte-vector ref) +DATA-HIVE+)))
   (if (null data)
       (progn (setf (return-code*) +http-not-found+) "")
       data)
  )
 )
)

; POST to /data will create a CAS-entry with the post body and return to the caller
; the 40-character stringized SHA1 hash of the content.
(defun data-handle-post ()
 (let* ((raw-json-string (octets-to-string (raw-post-data :request *request*) :external-format :utf8))
        (json (if (or (null raw-json-string) (string= raw-json-string "")) 
                  nil 
                  (json:decode-json-from-string raw-json-string))))
  (if (null json)
   (setf (return-code*) +http-bad-request+)
   (let ((data (cdr (assoc :data json))))
    (if (null data)
        (setf (return-code*) +http-bad-request+)
        (byte-vector-to-hex-string (data-store-string data))
    )
   )
  )
 )
)

(defun data-handler (uri)
 (let ((ref (cadr uri))
       (req (request-method* *request*)))
  (cond ((eq req :get) (data-handle-get ref))
        ((eq req :post) (data-handle-post))
        (t (setf (return-code*) +http-method-not-allowed+)))
 )
)

