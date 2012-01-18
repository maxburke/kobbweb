(in-package :kobbweb)

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

