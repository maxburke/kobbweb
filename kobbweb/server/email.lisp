(in-package :kobbweb)

(declaim (inline email-trim-and-resolve))
(defun email-trim-and-resolve (str user-id)
 (to-uuid user-id (subseq str 0 (position #\@ str)))
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
 (let* ((user-id (email-retrieve-user-id (post-parameter "from")))
        (uuid (email-trim-and-resolve (post-parameter "to") user-id))
        (data (post-parameter "data")))
  (email-handle-insertion uuid user-id data)
  *successful-post-response*
 )
)

; The email client only handles POST methods to /email. The request
; must consist of a JSON object with the following schema:
; "to" : "<uuid-or-alias@kobbweb.net>"
;   * this field must be either a valid alias or a valid uuid
; "from" : "<users-email-address>"
;   * the users email address here must be a valid Kobbweb user and
;     they must have permissions to access the item identified by the
;     uuid/alias above.
; "data" : "<body of email message>"
;   * this contains the raw body of the email. No assumptions are made
;     about the content of this field, other than it must be a string.
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
