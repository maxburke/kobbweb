(in-package :kobbweb)

; As the basis of this primitive CAS this macro opens a file in the given direction
; with the provided name. This takes care of the conversion of the byte vector to
; name string, opening the file, and closing the file after it is finished.
(defmacro with-hex-named-file ((direction does-not-exist exists root name-buffer stream file-path-var) &body body)
 (let ((file-name-var (gensym)))
  `(let* ((,file-name-var (byte-vector-to-hex-string ,name-buffer))
          (,file-path-var (make-pathname :directory (list :relative "data" ,root) :name ,file-name-var))
          (,stream (open ,file-path-var
               :element-type '(unsigned-byte 8) 
               :direction ,direction 
               :if-does-not-exist ,does-not-exist 
               :if-exists ,exists)))
    ,@body
    (if (not (null ,stream))
        (close ,stream))
   )
 )
)

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

; Load the data associated with content-ref from the specified hive, or returns
; nil if the ref is invalid.
(defun cas-load (content-ref hive)
 (let ((content))
  (with-hex-named-file (:input nil nil hive content-ref file file-name)
   (let* ((content-length (file-length file))
          (content-storage (make-array content-length :element-type '(unsigned-byte 8))))
    (assert (not (null content-length)))
    (read-sequence content-storage file)
    (setf content content-storage)
   )
  )
  content
 )
)

; Store the given content, provided as a byte vector, in the given hive. Returns
; the SHA1 digest of the content.
(defun cas-store (content hive)
 (let ((content-ref))
  (with-sha1-digest (digest content)

   ; Data store files are set to create if they do not exist and return nil if they do
   (with-hex-named-file (:output :create nil hive digest stream file-name)
    (setf content-ref digest)

    ; If we have a valid file stream then write the contents to it.
    (if (not (null stream))
        (write-sequence content stream)

     ; However, if we don't have a valid file stream ensure that we it does exist.
     ; If the file doesn't exist then set the returned content ref to nil. This is
     ; a bad condition.
     (if (null (probe-file file-name))
         (setf content-ref nil)))
   )
  )
  content-ref
 )
)

