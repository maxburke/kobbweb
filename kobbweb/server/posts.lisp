(in-package :kobbweb)

(defprepared select-most-recent
 (:limit (:order-by (:select 'item_id :from 'posts :where (:= 'user_id '$1)) (:desc 'post_id)) '$2 '$3))

; Fetch the 10 most recent posts by the user.
(defun posts-fetch (user-id)
 (with-connection *db-connection-parameters*
  (let* ((posts (select-most-recent user-id 10 0))
         (simplified-list (mapcar (lambda (post) 
                                   (car (fetch-single-item-content-summary (hex-string-to-byte-vector (car post)))))
                           posts)))
   (json:encode-json-to-string simplified-list)
  )
 )
)

(defun posts-handler (uri)
 (if *session*
  (posts-fetch (session-value 'id *session*))
  (setf (return-code*) +http-forbidden+)
 )
)
