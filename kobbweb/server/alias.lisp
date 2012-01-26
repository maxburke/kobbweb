(in-package :kobbweb)

(defun alias-resolve-uuid (user-id alias)
 (with-connection *db-connection-parameters*
  (let* ((rows (query (:select 'item_id :from 'aliases
                        :where (:and (:= 'user_id user-id) (:= 'alias alias))))))
   (if (null rows)
       nil
       (caar rows))
  )
 )
)

(defun alias-resolve-alias (user-id uuid)
 (with-connection *db-connection-parameters*
  (let* ((rows (query (:select 'alias :from 'aliases
                        :where (:and (:= 'user_id user-id) (:= 'item_id uuid))))))
   (if (null rows)
       nil
       (caar rows))
  )
 )
)

(defun alias-set (user-id uuid alias)
 (with-connection *db-connection-parameters*
  (if (null (alias-resolve-alias user-id uuid))
      (execute
        (:insert-into 'aliases :set 'user_id user-id 'item_id uuid 'alias alias))
      (execute
        (:update 'aliases :set 'alias alias :where (:and (:= 'user_id user-id) (:= 'item_id uuid)))))
 )
)

; Duplicates an alias existing for the user, user-id, for the new user,
; new-user-id. This is called when a user is added to the ACL of a particular
; item.
(defun alias-duplicate (user-id new-user-id uuid)
 (with-connection *db-connection-parameters*
  (let* ((rows (query (:select 'alias :from 'aliases 
                        :where (:and (:= 'user_id user-id) (:= 'item_id uuid)))))
         (alias (caar rows)))
   (when (not (null rows))
        (execute 
            (:insert-into 'aliases :set 'user_id new-user-id 'item_id uuid 'alias alias)))
  )
 )
)

(defun alias-handle-get (uuid-string)
 (with-connection *db-connection-parameters*
  (let* ((user-id (session-value 'id *session*))
         (rows (query (:select 'alias :from 'aliases
                       :where (:and (:= 'user_id user-id) (:= 'item_id uuid-string)))))
         (id-string (if (null (caar rows))
                        uuid-string
                        (caar rows)))
         (json-string (encode-json-to-string `((:alias . ,id-string)))))
  json-string)
 )
)

(defun alias-handle-post (uuid-or-alias-string)
 (let* ((user-id (session-value 'id *session*))
        (raw-json-string (octets-to-string (raw-post-data :request *request*) :external-format :utf8))
        (json (if (or (null raw-json-string) (string= raw-json-string ""))
                  nil
                  (json:decode-json-from-string raw-json-string)))
        (alias (cdr (assoc :alias json))))
  (alias-set user-id uuid-or-alias-string alias)
  *successful-post-response*
 )
)

(defun alias-handler (uri)
 (let ((req (request-method* *request*))
       (uuid-or-alias-string (cadr uri)))
  (if (not (null uuid-or-alias-string))
      (progn (cond ((eq req :get) (alias-handle-get uuid-or-alias-string))
                   ((eq req :post) (alias-handle-post uuid-or-alias-string))))
      (setf (return-code*) +http-bad-request+))
 )
)

