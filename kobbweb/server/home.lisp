(in-package :kobbweb)

(defun home-view ()
 (if (null *session*)
  (setf (return-code*) +http-authorization-required+)
  (progn (let* ((user-id (session-value 'id *session*))
                (events-list (events-fetch-json user-id)))
          events-list
         )
  )
 )
)

