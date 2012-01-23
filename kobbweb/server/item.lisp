(in-package :kobbweb)

;;;; UUID generation and comparison.
;;; UUIDs are all represented as byte vectors. Always.
(defvar *uuid-lock* (sb-thread:make-mutex :name "uuid lock"))
(defvar *null-uuid* (uuid:uuid-to-byte-array (uuid:make-null-uuid)))
(defvar *null-digest*)
(defvar *last-uuid* *null-uuid*)

(defparameter +LIST-HIVE+ "lists")
(defparameter +ACL-HIVE+ "acl_list")
(defparameter +DATA-HIVE+ "data")


(declaim (inline uuid=))
(defun uuid= (uuid1 uuid2)
 (reduce (lambda (x y) (and x y)) (map 'vector #'= uuid1 uuid2))
)

(defvar *hex-chars* "0123456789abcdef")
(defun byte-vector-to-hex-string (byte-vector)
 (let ((output (make-array (* 2 (length byte-vector)) :element-type 'character))
       (idx 0))
  (loop for b across byte-vector
   do
   (let ((high-nybble (logand (ash b -4) 15))
         (low-nybble (logand b 15)))
    (setf (aref output idx) (char *hex-chars* high-nybble))
    (setf (aref output (1+ idx)) (char *hex-chars* low-nybble))
    (setf idx (+ 2 idx))
   )
  )
  (coerce output 'string)
 )
)

(defun hex-string-to-byte-vector (string)
 (let* ((string-length (length string))
        (output (make-array (ash string-length -1) :element-type '(unsigned-byte 8))))
  (loop for i from 0 to (1- string-length) by 2
   do
   (let* ((high-nybble (position (char string i) *hex-chars* :test #'equalp))
          (low-nybble (position (char string (1+ i)) *hex-chars* :test #'equalp))
          (vector-index (ash i -1))
          (byte (logior (ash (logand high-nybble 15) 4) (logand low-nybble 15))))
    (setf (aref output vector-index) byte)
   )
  )
  output
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

(defun fixnum-to-word-bytes (num)
 (declare (type fixnum num))
 (let ((seq (make-array 4 :element-type '(unsigned-byte 8))))
  (setf (aref seq 0) (logand num 255))
  (setf (aref seq 1) (logand (ash num -8) 255))
  (setf (aref seq 2) (logand (ash num -16) 255))
  (setf (aref seq 3) (logand (ash num -24) 255))
  seq)
)

(defun word-bytes-to-fixnum (bytes)
 (logior (aref bytes 0) 
         (ash (aref bytes 1) 8)
         (ash (aref bytes 2) 16)
         (ash (aref bytes 3) 24))
)

(defconstant +word-size+ 4)
(defconstant +uuid-size+ 16)
(defconstant +sha1-size+ 20)

(declaim (inline make-item))
(defstruct item
 (schema-version 1 :type fixnum)
 (uuid #() :type vector)
 (acl-ref #() :type vector)
 (list-ref #() :type vector)
 (parent-uuid #() :type vector)
 (content-ref #() :type vector)
 (user-id 0 :type fixnum)
)

(defun item-write-to-stream (item stream)
 (let ((raw-user-id (fixnum-to-word-bytes (item-user-id item)))
       (raw-schema-id (fixnum-to-word-bytes (item-schema-version item))))
  (write-sequence raw-schema-id stream)
  (write-sequence raw-user-id stream)
  (write-sequence (item-acl-ref item) stream)
  (write-sequence (item-list-ref item) stream)
  (write-sequence (item-parent-uuid item) stream)
  (write-sequence (item-content-ref item) stream)
 )
)

(defun item-read-from-stream (uuid stream)
 (if (not (null stream))
     (let ((item (make-item))
           (acl-ref (make-array +sha1-size+ :element-type '(unsigned-byte 8)))
           (list-ref (make-array +sha1-size+ :element-type '(unsigned-byte 8)))
           (parent-uuid (make-array +uuid-size+ :element-type '(unsigned-byte 8)))
           (content-ref (make-array +sha1-size+ :element-type '(unsigned-byte 8)))
           (user-id (make-array +word-size+ :element-type '(unsigned-byte 8)))
           (schema-version (make-array +word-size+ :element-type '(unsigned-byte 8)))
          )
      (read-sequence schema-version stream :start 0 :end +word-size+)
      (read-sequence user-id stream :start 0 :end +word-size+)
      (read-sequence acl-ref stream :start 0 :end +sha1-size+)
      (read-sequence list-ref stream :start 0 :end +sha1-size+)
      (read-sequence parent-uuid stream :start 0 :end +uuid-size+)
      (read-sequence content-ref stream :start 0 :end +sha1-size+)
      (setf (item-uuid item) uuid)
      (setf (item-acl-ref item) acl-ref)
      (setf (item-list-ref item) list-ref)
      (setf (item-parent-uuid item) parent-uuid)
      (setf (item-content-ref item) content-ref)
      (setf (item-user-id item) (word-bytes-to-fixnum user-id))
      (setf (item-schema-version item) (word-bytes-to-fixnum schema-version))
      item
     )
     nil
 )
)

(defun acl-create-default (user-id)
 (let ((bytes (fixnum-to-word-bytes user-id)))
  (cas-store bytes +ACL-HIVE+)
 )
)

(defun acl-is-member-of (acl-id user-id)
 (let ((result))
  (with-hex-named-file (:input nil nil +ACL-HIVE+ acl-id file file-name)
   (let ((scratch (make-array 4 :element-type '(unsigned-byte 8))))
    (labels ((recursive-acl-is-member-of (user-id file)
              (read-sequence scratch file)
              (if (= (word-bytes-to-fixnum scratch) user-id)
                  t
                  (recursive-acl-is-member-of user-id file))))
     (setf result (recursive-acl-is-member-of user-id file))
    )
   )
  )
  result
 )
)


(defun data-load (content-ref)
 (cas-load content-ref +DATA-HIVE+)
)

(defun data-store-octets (content)
 (cas-store content +DATA-HIVE+)
)

(defun data-store-string (string)
 (let ((octets (flexi-streams:string-to-octets string)))
  (data-store-octets octets)
 )
)

(defun item-store (item)
 (with-hex-named-file (:output :create :supersede "items" (item-uuid item) file file-name)
  (item-write-to-stream item file)
 )
)

(defun item-load (uuid)
 (let ((item))
  (with-hex-named-file (:input nil nil "items" uuid file file-name)
   (if (not (null file))
       (setf item (item-read-from-stream uuid file))))
  item
 )
)

(defun item-get-acl-from-parent (parent-uuid)
 (let ((item (item-load parent-uuid)))
  (if (not (null item))
      (item-acl-ref item)
  )
 )
)

(declaim (inline memcpy))
(defun memcpy (dest src dest-offset src-offset size)
 (assert (<= (+ size src-offset) (length src)))
 (assert (<= (+ size dest-offset) (length dest)))
 (loop for i from 0 to (1- size)
  do
  (setf (aref dest (+ i dest-offset)) (aref src (+ i src-offset)))
 )
)

(defun item-get-list-as-strings (item)
 (let ((children '())
       (list-bytes (cas-load (item-list-ref item) +LIST-HIVE+))
       (item (make-array +uuid-size+ :element-type '(unsigned-byte 8))))
  (loop for i from 0 to (1- (length list-bytes)) by +uuid-size+
   do
   (memcpy item list-bytes 0 i +uuid-size+)
   (push (byte-vector-to-hex-string item) children)
  )
  children
 )
)

(defun item-add-to-list (item-uuid child-uuid)
 ;; TODO: This isn't entirely atomic and will need to be revisited later.
 ;; This function should check the list for the child uuid and if it can't
 ;; be found then add it, in a loop, in case some other source is modifying
 ;; this same structure.
 (let* ((item (item-load item-uuid))
        (list (cas-load (item-list-ref item) +LIST-HIVE+))
        (new-list (make-array (+ (length list) +uuid-size+) :element-type '(unsigned-byte 8)))
        (new-list-ref))
  (memcpy new-list child-uuid 0 0 +uuid-size+)
  (memcpy new-list list +uuid-size+ 0 (length list))
  (setf new-list-ref (cas-store new-list +LIST-HIVE+))
  (setf (item-list-ref item) new-list-ref)
  (item-store item)
 )
)

(defun item-remove-from-list (item-uuid child-uuid)
 ;; As above, this function isn't atomic. Gonna have to work on that later.
 ;; Perhaps use a proper schema-less database.
 (let* ((item (item-load item-uuid))
        (list (cas-load (item-list-ref item) +LIST-HIVE+))
        (new-list (make-array (- (length list) +uuid-size+) :element-type '(unsigned-byte 8)))
        (temp-list (make-array +uuid-size+ :element-type'(unsigned-byte 8)))
        (dest-idx 0)
        (list-length (length list))
        (new-list-ref))
  (loop for i from 0 to (1- list-length) by +uuid-size+
   do
   (memcpy temp-list list 0 i +uuid-size+)
   (unless (uuid= temp-list child-uuid)
           (progn (memcpy new-list list dest-idx i +uuid-size+)
                  (setf dest-idx (+ dest-idx +uuid-size+))))
  )
  (setf new-list-ref (cas-store new-list +LIST-HIVE+))
  (setf (item-list-ref item) new-list-ref)
  (item-store item)
 )
)

(let ((null-content (make-array 0 :element-type '(unsigned-byte 8))))
 (with-sha1-digest (digest null-content)
  (setf *null-digest* digest)
  (cas-store null-content +LIST-HIVE+)
  (cas-store null-content +ACL-HIVE+)
  (cas-store null-content +DATA-HIVE+)
 )
)

(defun item-create (user-id parent-uuid content-ref)
 (let ((acl-ref nil)
       (uuid (uuid-next)))

  (if (or (null parent-uuid) (uuid= parent-uuid *null-uuid*))
      (progn (setf parent-uuid *null-uuid*)
             (setf acl-ref (acl-create-default user-id)))
      (progn (setf acl-ref (item-get-acl-from-parent parent-uuid)))
  )

  (if (null content-ref)
      (setf content-ref *null-digest*))

  (let ((item (make-item 
               :uuid uuid
               :acl-ref acl-ref
               :list-ref *null-digest*
               :parent-uuid parent-uuid
               :content-ref content-ref
               :user-id user-id)))
   (item-store item)
   (when (not (uuid= parent-uuid *null-uuid*))
         (item-add-to-list parent-uuid uuid)
   )
   item)
 )
)

