(in-package :kobbweb)

(declaim (inline email-trim-and-resolve))
(defun email-trim-and-resolve (str)
 (to-uuid (subseq str 0 (position #\@ str)))
)

(defun email-retrieve-user-id (email)
 (with-connection *db-connection-parameters*
  (let ((user-id (fetch-user-id email)))
   user-id
  )
 )
)

(defun email-handle-insertion (uuid user-id data)
 (let* ((content-ref (data-store-string data))
        (item (item-create user-id uuid content-ref)))
  (content-record-new-post user-id item)
 )
)

(defun email-handle-post ()
  (let ((post-data (raw-post-data :request *request*)))
   (if (or (null post-data) (string= post-data ""))
       (progn
            (setf (return-code*) +http-bad-request+)
            "Bad request!")
       (let* ((json (json:decode-json-from-string post-data))
              (uuid (email-trim-and-resolve (cdr (assoc :to json))))
              (user-id (email-retrieve-user-id (cdr (assoc :from json))))
              (data (cdr (assoc :data json))))
        (email-handle-insertion uuid user-id data)
        *successful-post-response*
       )
   )
  )
)

(defun email-handler ()
 (let ((req (request-method* *request*)))
  (if (eq req :post)
      (email-handle-post)
      (progn
            (setf (return-code*) +http-method-not-allowed+)
            "Method not allowed!")
  )
 )
)
