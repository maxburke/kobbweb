(in-package :kobbweb)
    
(defun create-failure-response (reason)
 (json:encode-json-to-string `((:success . "false") (:reason . ,reason)))
)

(defvar *successful-login-response* (json:encode-json-to-string '((:success . "true"))))
(defvar *invalid-password-response* (create-failure-response "I don't think that's your password!"))

(defprepared valid-password-for-p
 (:select (:= 'password (:crypt '$2 'password)) :from 'users :where (:= '$1 'email)) :single)

(defprepared fetch-user-id
 (:select 'user_id :from 'users :where (:= '$1 'email)) :single)
                                                                                      
(defun login-and-start-session (email)
 (with-connection *db-connection-parameters*
  (let ((session (start-session))
        (user-id (fetch-user-id email)))
   (setf (session-value 'id session) user-id)
   (setf (session-value 'email session) email)
  )
 )
)

(defun raw-login-data ()
 (let ((raw-json-string (octets-to-string (raw-post-data :request *request*) :external-format :utf8)))
  (if (and (not (null raw-json-string)) (not (string= raw-json-string "")))
   raw-json-string
   nil)
 )
)

(defun extract-login-data (raw-json-string)
 (let* ((json-string (json:decode-json-from-string raw-json-string))
        (email (cdr (assoc :email json-string)))
        (password (cdr (assoc :password json-string)))
        (password-verify (cdr (assoc :password-verify json-string))))
  (values email password password-verify)
 )
)

(defun prepare-failure-response (join-failure-cause)
 (let* ((failure-cause-string 
          (cond ((eq join-failure-cause 'email-exists) "This email address is already in use!")
                ((eq join-failure-cause 'bad-email) "The email address is invalid!")
                ((eq join-failure-cause 'bad-password) "The password is too common!")
                ((eq join-failure-cause 'password-too-short) "The password is too short! It must be at least 5 characters.")
                ((eq join-failure-cause 'password-match) "The passwords don't match!")
                (t "Unknown error! Please contact us above and report this.")))
        (response (create-failure-response failure-cause-string)))
  response)
)

(defmacro with-valid-login-data ((email password password-verify) &body body)
 (let ((var (gensym)))
  `(let ((,var (raw-login-data)))
    (if (null ,var)
     (progn (setf (return-code*) +http-bad-request+)
            (create-failure-response "It seems like you missed your email address or password!"))
     (progn (multiple-value-bind (,email ,password ,password-verify) (extract-login-data ,var)
      ,@body))
    )
   )
 )
)

(defun login-handle-post ()
 (if *session*
  *successful-login-response*
  (with-valid-login-data (email password password-verify)
   (let ((join-failure-cause (establish-join-failure-cause email password password-verify)))
    (if (eq join-failure-cause 'success)
     (progn (create-new-user email password)
            (login-and-start-session email)
            *successful-login-response*)
     (progn (setf (return-code*) +http-bad-request+)
            (prepare-failure-response join-failure-cause))
    )
   )
  )
 )
)

(defun login-handle-put ()
 (if *session*
  *successful-login-response*
  (with-connection *db-connection-parameters*
   (with-valid-login-data (email password password-verify)
    (if (valid-password-for-p email password)
     (progn (login-and-start-session email)
            *successful-login-response*)
     (progn (setf (return-code*) +http-forbidden+)
            *invalid-password-response*)
    )
   )
  )
 )
)

(defun login-handle-delete ()
 (if *session*
  (progn (delete-session-value 'id)
         (remove-session *session*)
         *successful-login-response*
  )
  (progn (setf (return-code*) +http-bad-request+)
         (create-failure-response "I don't think you're logged in!"))
 )
)

(defun sessions-handler ()
 (let ((req (request-method* *request*)))
  (cond ((eq req :post) (login-handle-post))
        ((eq req :put) (login-handle-put))
        ((eq req :delete) (login-handle-delete))
  )
 )
)

(defun login-handler ()
 (if *session*
  (redirect "/home")
  (handle-static-file #p"static/login.html")
 )
)

