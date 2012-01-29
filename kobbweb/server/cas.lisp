(in-package :kobbweb)

; Creates a SHA1 digest, digest, from the given byte vector buffer and executes
; the provided body.
(defmacro with-sha1-digest ((digest buffer) &body body)
 (let ((digester-var (gensym)))
  `(let ((,digester-var (ironclad:make-digest :sha1)))
    (ironclad:update-digest ,digester-var ,buffer)
    (let ((,digest (ironclad:produce-digest ,digester-var)))
     ,@body
    )
   )
 )
)

(defun kv-store (hive key value)
 (with-connection *db-connection-parameters*

  ; Since data is uniquely identified by its SHA1 hash a duplicate value 
  ; will have the same key as the pre-existing record. We can just ignore
  ; the errors the database gives us here regarding duplicate primary keys.
  (ignore-errors (execute (:insert-into hive :set 'key key 'value value)))
 )
)

(defun kv-load (hive key)
 (with-connection *db-connection-parameters*
  (let ((rows (query (:select 'value :from hive :where (:= 'key key)))))
   (if (null rows)
       nil
       (caar rows))
  )
 )
)

; Load the data associated with content-ref from the specified hive, or returns
; nil if the ref is invalid.
(defun cas-load (content-ref hive)
 (kv-load hive content-ref)
)

; Store the given content, provided as a byte vector, in the given hive. Returns
; the SHA1 digest of the content.
(defun cas-store (content hive)
 (let ((content-ref))
  (with-sha1-digest (digest content)
   (kv-store hive digest content)
   (setf content-ref digest)
  )
  content-ref
 )
)

