(in-package :kobbweb)

;;;; UUID generation and comparison.
;;; UUIDs are all represented as byte vectors. Always.
(defvar *uuid-lock* (sb-thread:make-mutex :name "uuid lock"))
(defvar *null-uuid* (uuid:uuid-to-byte-array (make-null-uuid)))
(defvar *hex-chars* "0123456789abcdef")
(defvar *last-uuid* *null-uuid*)

(defun uuid= (uuid1 uuid2)
 (reduce (lambda (x y) (and x y)) (map 'vector #'= uuid1 uuid2))
)

(defun uuid-to-string (uuid)
 (let ((output (make-array 32 :fill-pointer 0)))
  (loop for b across uuid
   do
   (let ((high-nybble (logand (ash b -4) 15))
         (low-nybble (logand b 15)))
    (vector-push (char *hex-chars* high-nybble) output)
    (vector-push (char *hex-chars* low-nybble) output)
   )
  )
  (coerce output 'string)
 )
)

(defun uuid-next ()
 (let ((uuid))
  (sb-thread:with-mutex (*uuid-lock*)
   (labels ((recursive-create-next-uuid ()
             (setf uuid (uuid:uuid-to-byte-array (uuid:make-v1-uuid)))
             (if (uuid= uuid *last-uuid*)
              (recursive-create-next-uuid)
              (setf *last-uuid* uuid))))
    (recursive-create-next-uuid)
   )
  )
  uuid
 )
)

(defun item-fetch (uuid)
)

(defun acl-create-default (user-id)
)

(defun item-get-acl-from (parent-uuid)
)

(defun user-id-to-bytes (user-id)
 (let ((seq (make-array 4 :fill-pointer 0)))
  (vector-push (logand user-id 255) seq)
  (vector-push (logand (ash user-id -8) 255) seq)
  (vector-push (logand (ash user-id -16) 255) seq)
  (vector-push (logand (ash user-id -24) 255) seq)
  seq)
)

(defun bytes-to-user-id (bytes)
 (logior (aref bytes 0) 
         (ash (aref bytes 1) 8)
         (ash (aref bytes 2) 16)
         (ash (aref bytes 3) 24))
)

(defun item-create (user-id parent-uuid content-ref)
 (let ((acl-uuid nil)
       (uuid (uuid-next)))

  (if (null parent-uuid)
   (progn (setf parent-uuid *null-uuid*)
          (setf acl-uuid (acl-create-default))
   )
   (progn (setf acl-uuid (item-get-acl-from parent-uuid))
   )
  )
  (if (null content-ref)
   (setf content-ref (make-array 20)))

  (let* ((uuid-file-name (uuid-to-string uuid))
         (file-path (make-pathname :directory '(:relative "data" "items") :name uuid-file-name))
         (file (open file-path :element-type '(unsigned-byte 8) :direction :output :if-does-not-exist :create :if-exists nil))
         (raw-user-id (user-id-to-bytes user-id))
         (raw-acl-uuid (uuid-to-byte-array acl-uuid))
         (raw-parent-uuid (uuid-to-byte-array parent-uuid))
        )
   (write-sequence raw-user-id file)
   (write-sequence raw-acl-uuid file)
   (write-sequence raw-parent-uuid file)
   (write-sequence content-ref file)
   (close file)
  )
  uuid
 )
)

