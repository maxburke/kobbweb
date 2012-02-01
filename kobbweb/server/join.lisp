(in-package :kobbweb)

;;;
(defun report-failure-message (html-stream join-failure-cause)
 (if (not (eq join-failure-cause 'none))
  (with-html-output (html-stream)
   (:div :id "error-box"
    (:h2 "There seems to have been a problem!"))
   (:div :id "error-message"
    (cond ((eq join-failure-cause 'email-exists)
             ; Should probably redirect the user to the lost-password link here.
             (htm (:h3 "This email address is already in use!")))
          ((eq join-failure-cause 'bad-email)
             (htm (:h3 "I don't think that email is correct!")))
          ((eq join-failure-cause 'bad-password)
             (htm (:h3 "The password is too common!")))
          ((eq join-failure-cause 'password-too-short)
             (htm (:h3 "The password is too short!")))
          ((eq join-failure-cause 'password-match) 
             (htm (:h3 "Your passwords didn't match!")))
    )
   )
  )
 )
)

;;;
(defun default-email-text ()
 (let ((email (post-parameter "email")))
  (if email
   email
   "")
 )
)

;;;
(defprepared email-exists-p
 (:select 'email :from 'users :where (:= '$1 'email)))

;;;
(defun establish-join-failure-cause (email password password-verify)
 (with-connection *db-connection-parameters*
  (cond ((null password) 'none)
        ((email-exists-p email) 'email-exists)
        ((not (string= password password-verify)) 'password-match)
        ((bad-password-p password) 'bad-password)
        ((< (length password) 5) 'password-too-short)
        ((not (find #\@ email)) 'bad-email)
        (t 'success)
  )
 )
)

;;;
(defun create-new-user (email password)
 (with-connection *db-connection-parameters*
  (execute (:insert-into 'users :set 'email email 'password (:crypt password (:gen_salt "bf"))))
  (let* ((rows (query (:select 'user_id :from 'users :where (:= 'email email))))
         (user-id (caar rows)))
   user-id)
 )
)


