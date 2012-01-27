(in-package :kobbweb)

(defprepared select-most-recent
 (:limit (:order-by (:select 'item_id :from 'posts) (:desc 'post_id)) '$1 '$2))

; Fetch the 10 most recent posts by the user.
(defun posts-fetch (user-id)
 (with-connection *db-connection-parameters*
  (let* ((posts (select-most-recent 10 0))
         (simplified-list (mapcar #'car posts))
         (json (json:encode-json-to-string simplified-list)))
   json
  )
 )
)

(defun posts-handler (uri)
 (if *session*
  (posts-fetch (session-value 'id *session*))
  (setf (return-code*) +http-forbidden+)
 )
)
