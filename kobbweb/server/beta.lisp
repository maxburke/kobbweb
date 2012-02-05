(in-package :kobbweb)

(defun beta-handle-post ()
 (with-posted-json (json)
  (let ((email (cdr (assoc :email json))))
   (with-connection *db-connection-parameters*
    (execute (:insert-into 'invite_requests :set 'email email))

    ;; TODO: Change this so that it doesn't use your home email!
    (cl-smtp:send-email "smtp.gmail.com"
                        "max@irsvp.cc"
                        email
                        "Thank you for your interest in kobbweb!"
                        (format nil "Thank you for your interest in kobbweb! As we are in a closed beta-testing stage we aren't able to process your request immediately but will do so as soon as possible. Once your account has been created you will receive an email with your login details.~%~%Cheers!~%-Max Burke~%kobbweb.net")
                        :ssl :tls
                        :authentication '(:login "max@irsvp.cc" "W0zixege"))

    *successful-post-response*
   )
  )
 )
)

(defun beta-handler ()
 (let* ((req (request-method* *request*)))
  (cond ((eq req :post) (beta-handle-post))
        (t (progn (setf (return-code*) +http-bad-request+)
                  "Bad request!"))
  )
 )
)
