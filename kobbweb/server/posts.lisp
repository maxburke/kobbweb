(in-package :kobbweb)

(defprepared select-most-recent
 (:limit (:order-by (:select 'item_id :from 'posts) 'post_id) '$1 '$2))

(defun posts-fetch (user-id)
 (with-connection *db-connection-parameters*
  (let* ((posts (select-most-recent 10 0))
         (simplified-list (mapcar #'car posts))
         (json (json:encode-json-to-string simplified-list)))
   json
  )
 )
)

(defun posts-handler ()
 (if *session*
  (posts-fetch (session-value 'id *session*))
  (setf (return-code*) +http-forbidden+)
 )
)