(in-package :kobbweb)

; Returns t if the char c is in the range [0..9] or [a..f]
(declaim (inline is-hex-char) (optimize (debug 0) (speed 3) (safety 0)))
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

(defun content-get-user-id (json-data)
 (if *session*
  (session-value 'id *session*)
  (let ((email (cdr (assoc :email json-data))))
   (if (null email)
       nil
       (with-connection *db-connection-parameters*
        (fetch-user-id email))
   )
  )
 )
)

; Create a JSON representation of the provided item structure.
(defun create-json-item (item)
 (let ((json-assoc '()))
  (push '(:success . "true") json-assoc)
  (push `(:parent . ,(byte-vector-to-hex-string (item-parent-uuid item))) json-assoc)
  (push `(:content-ref . ,(byte-vector-to-hex-string (item-content-ref item))) json-assoc)
  (push `(:children . ,(item-get-list-as-strings item)) json-assoc)
  (push `(:id . ,(byte-vector-to-hex-string (item-uuid item))) json-assoc)
  (json:encode-json-to-string json-assoc)
 )
)

; GET /content/<uuid> returns the item with the given uuid. In this case
; uuid is a byte vector, already resolved by to-uuid, and converted. It
; is expected that the json post body here is null.
(defun content-handle-get (uuid json-data)
 (assert (null json-data))
 (let ((item (item-load uuid))
       (user-id (content-get-user-id json-data)))
  (if (null user-id)
      (progn
            (setf (return-code*) +http-forbidden+)
            "Forbidden!")
      (if (null item)
          (progn
                (setf (return-code*) +http-not-found+)
                "Not found!")
          (if (acl-is-member-of (item-acl-ref item) user-id)
              (create-json-item item)
              (progn
                    (setf (return-code*) +http-forbidden+)
                    "Forbidden!")
          )
      )
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
(defun content-handle-post (uuid json-data)
 (assert (not (null json-data)))
 (let* ((user-id (content-get-user-id json-data))
        (content-ref (hex-string-to-byte-vector (cdr (assoc :data json-data))))
        (item (item-create user-id uuid content-ref)))
  (content-record-new-post user-id item)
  (create-json-item item)
 )
)

; DELETE to /content/parentuuid requires a JSON structure with member 
; child : "<childuuid>" and de-links child from parent. The item isn't
; removed from the system as it may have been linked to other parent
; items.
(defun content-handle-delete (parent-uuid json-data user-id)
 (let* ((child (cdr (assoc :child json-data)))
        (child-uuid (to-uuid user-id child)))
  (if (item-remove-from-list parent-uuid child-uuid)
      *successful-post-response*
      (setf (return-code*) +http-bad-request+))
 )
)

(defun content-handler (uri)
 (let* ((req (request-method* *request*))
        (uuid-or-alias-string (cadr uri))
        (user-id (session-value 'id *session*))
        (uuid (if (not (null uuid-or-alias-string))
                  (to-uuid user-id uuid-or-alias-string)
                  *null-uuid*))
        (raw-json-string (octets-to-string (raw-post-data :request *request*) :external-format :utf8))
        (json-post-data (if (or (null raw-json-string) (string= raw-json-string "")) 
                            nil
                            (json:decode-json-from-string raw-json-string))))
  (cond ((eq req :get) (content-handle-get uuid json-post-data))
        ((eq req :post) (content-handle-post uuid json-post-data))
        ((eq req :delete) (content-handle-delete uuid json-post-data user-id))
        (t (progn 
                 (setf (return-code*) +http-bad-request+)
                 "Bad request!"))
  )
 )
)

