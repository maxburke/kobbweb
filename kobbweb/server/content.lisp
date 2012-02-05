(in-package :kobbweb)

; Returns t if the char c is in the range [0..9] or [a..f]
(declaim (inline is-hex-char))
(defun is-hex-char (c)
 (declare (type character c))
 (let ((code (char-code c)))
  (or (and (>= code (char-code #\0)) (<= code (char-code #\9)))
      (and (>= code (char-code #\a)) (<= code (char-code #\f))))
 )
)

; Returns true if the provided string only contains hex chars. This
; is used to determine if a string is either a uuid or an alias. 
; Hex strings 32-chars long are uuids, all else are treated as aliases.
(declaim (inline contains-only-hex-chars))
(defun contains-only-hex-chars (string)
 (declare (type simple-string string))
 (labels ((recursive-contains-only-hex-chars (str i length)
           (declare (type fixnum i length)
                    (type simple-string str))
           (if (= i length)
               t
               (if (is-hex-char (char str i))
                   (recursive-contains-only-hex-chars str (1+ i) length)
                   nil))))
  (recursive-contains-only-hex-chars string 0 (length string)))
)

; Given a string that could either be an alias or uuid, return the uuid
; it resolves to, or nil if it's an invalid uuid/alias.
(defun to-uuid (user-id uuid-or-alias)
 (declare (type simple-string uuid-or-alias))
 (hex-string-to-byte-vector 
  (if (and (= (length uuid-or-alias) 32)
           (contains-only-hex-chars uuid-or-alias))
   uuid-or-alias
   (alias-resolve-uuid user-id uuid-or-alias)
  )
 )
)

(defun content-get-user-id (json)
 (if *session*
  (session-value 'id *session*)
  (let ((email (cdr (assoc :email json))))
   (if (null email)
       nil
       (with-connection *db-connection-parameters*
        (fetch-user-id email))
   )
  )
 )
)

; Create a JSON representation of the provided item structure.
(defun create-item-assoc (item list-bytes)
 (let ((json-assoc '()))
  (push `(:parent . ,(byte-vector-to-hex-string (item-parent-uuid item))) json-assoc)
  (push `(:content-ref . ,(byte-vector-to-hex-string (item-content-ref item))) json-assoc)
  (push `(:children . ,(item-get-list-as-strings list-bytes)) json-assoc)
  (push `(:id . ,(byte-vector-to-hex-string (item-uuid item))) json-assoc)
  json-assoc
 )
)

(defun create-json-item (item)
 (let ((list-bytes (cas-load (item-list-ref item) +LIST-HIVE+)))
  (json:encode-json-to-string (create-item-assoc item list-bytes))
 )
)

; fetch-children-content-summaries fetches the content records for a given
; item list (provided in byte vector form) but does not fetch the children
; of these items, returning it in alist form for jsonification.
(defun fetch-children-content-summaries (item-list-bytes)
 (let* ((item-list '())
        (temp-child (make-array +uuid-size+ :element-type '(unsigned-byte 8)))
        (children-last-index (1- (length item-list-bytes))))
  (loop for i from 0 to children-last-index by +uuid-size+
   do
   (memcpy temp-child item-list-bytes 0 i +uuid-size+)
   (let* ((child-item (item-load temp-child))

          ; For now we can get away with not resolving the children's list details
          ; which will save us time when grabbing stuff from the CAS.
;          (child-item-list (cas-load (item-list-ref child-item) +LIST-HIVE+))
          (child-assoc-list (create-item-assoc child-item #()))
          (child-uuid-string (cdr (assoc :id child-assoc-list))))
    (push child-assoc-list item-list)
   )
  )
  item-list
 )
)

; Similar to the above function, this fetches a content record, sans children list,
; and returns it in alist form.
(defun fetch-single-item-content-summary (item-uuid)
 (unless (uuid= item-uuid *null-uuid*)
  (let* ((item-list '())
         (parent-item (item-load item-uuid))
         (parent-assoc (create-item-assoc parent-item #()))
         (item-uuid (cdr (assoc :id parent-assoc))))
   (cons parent-assoc nil)
  )
 )
)

; GET /content/<uuid> returns the item with the given uuid. In this case
; uuid is a byte vector, already resolved by to-uuid, and converted. It
; is expected that the json post body here is null.
(defun content-handle-get (uuid)
 (let ((item (item-load uuid))
       (user-id (content-get-user-id nil)))
  (when (null user-id)
        (setf (return-code*) +http-forbidden+)
        (return-from content-handle-get "Forbidden!"))
  (when (null item)
        (setf (return-code*) +http-not-found+)
        (return-from content-handle-get "Not found!"))
  (unless (acl-is-member-of (item-acl-ref item) user-id)
          (setf (return-code*) +http-forbidden+)
          (return-from content-handle-get "Forbidden"))

  (let* ((item-list-bytes (cas-load (item-list-ref item) +LIST-HIVE+))
         (item-assoc (create-item-assoc item item-list-bytes))
         (item-uuid-string (cdr (assoc :id item-assoc))))
   (json:encode-json-to-string (nconc (fetch-children-content-summaries item-list-bytes) 
                                      (fetch-single-item-content-summary (item-parent-uuid item))
                                      (cons item-assoc nil)))
  )
 )
)

; Note in the database that a new post has been made.
(defun content-record-new-post (user-id item)
 (with-connection *db-connection-parameters*
  (execute (:insert-into 'posts :set 'user_id user-id 'item_id (byte-vector-to-hex-string (item-uuid item)))))
)

; POST to /content/uuid creates a new item with the item represented by uuid
; as its parent. This expects a JSON object with the member data : "<data>" at
; a minimum. This can also have a member email : "foo@bar.com" which is used
; by the email posting system as cookies/session data is not available. This
; returns a JSON representation of the item structure which is fed back to the
; client.
(defun content-handle-post (uuid)
 (with-posted-json (json)
  (assert (not (null json)))
  (let* ((user-id (content-get-user-id json))
         (content-ref (hex-string-to-byte-vector (cdr (assoc :data json))))
         (item (item-create user-id uuid content-ref)))
   (content-record-new-post user-id item)
   (create-json-item item)
  )
 )
)

; DELETE to /content/parentuuid requires a JSON structure with member 
; child : "<childuuid>" and de-links child from parent. The item isn't
; removed from the system as it may have been linked to other parent
; items.
(defun content-handle-delete (parent-uuid user-id)
 (with-posted-json (json)
  (let* ((child (cdr (assoc :child json)))
         (child-uuid (to-uuid user-id child)))
   (if (item-remove-from-list parent-uuid child-uuid)
       *successful-post-response*
       (setf (return-code*) +http-bad-request+))
  )
 )
)

(defun content-handler (uri)
 (let* ((req (request-method* *request*))
        (uuid-or-alias-string (cadr uri))
        (user-id (session-value 'id *session*))
        (uuid (if (not (null uuid-or-alias-string))
                  (to-uuid user-id uuid-or-alias-string)
                  nil)))
  (cond ((eq req :get) (content-handle-get uuid))
        ((eq req :post) (content-handle-post uuid))
        ((eq req :delete) (content-handle-delete uuid user-id))
        (t (progn 
                 (setf (return-code*) +http-bad-request+)
                 "Bad request!"))
  )
 )
)

