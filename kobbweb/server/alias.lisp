(in-package :kobbweb)

; Given a user id and an alias, return the associated item uuid, or nil
; if one does not exist.
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

; Analagous to the above, given a user id and a uuid in hex-string form,
; return the associated item alias, or nil if one does not exist.
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

; This function either creates a new alias, if one does not yet exist,
; or replaces an existing one with the alias provided.
(defun alias-set (user-id uuid alias)
 (with-connection *db-connection-parameters*
  (with-transaction ()
   (execute (:delete-from 'aliases :where (:and (:= 'user_id user-id) (:or (:= 'item_id uuid) (:= 'alias alias)))))
   (execute (:insert-into 'aliases :set 'user_id user-id 'item_id uuid 'alias alias))
  )
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

; GET /alias/<uuid> returns a JSON object { "alias" : "<alias string>" }
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

; POST to /alias/uuid of a json object { "alias" : "<alias string>" } either
; creates a new alias if one does not yet exist or updates an existing one
; for the associated user and item.
(defun alias-handle-post (uuid-or-alias-string)
 (with-posted-json (json)
  (let* ((user-id (session-value 'id *session*))
         (alias (cdr (assoc :alias json))))
   (alias-set user-id uuid-or-alias-string alias)
   *successful-post-response*
  )
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

