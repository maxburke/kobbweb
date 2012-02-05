(in-package :kobbweb)

; Typically items are created with a default ACL containing only the 
; posting user's ID number. This function creates that ACL.
(defun acl-create-default (user-id)
 (let ((bytes (fixnum-to-word-bytes user-id)))
  (cas-store bytes +ACL-HIVE+)
 )
)

; Tests to see if the given user ID is a member of a particular ACL.
; TODO: Use bloom filter?
(defun acl-is-member-of (acl-id user-id)

 ;; Null ACL ids are public so every user is a member of this ACL.
 (when (uuid= acl-id *null-digest*)
       (return-from acl-is-member-of t))

 (let* ((result)
        (acl (cas-load acl-id +ACL-HIVE+))
        (scratch (make-array 4 :element-type '(unsigned-byte 8)))
        (stream (flexi-streams:make-in-memory-input-stream acl)))
  (labels ((recursive-acl-is-member-of (user-id byte-stream)
            (if (zerop (read-sequence scratch byte-stream))
                (return-from recursive-acl-is-member-of nil))
            (if (= (word-bytes-to-fixnum scratch) user-id)
                t
                (recursive-acl-is-member-of user-id byte-stream))))
   (setf result (recursive-acl-is-member-of user-id stream))
  )
  result
 )
)

(defun acl-handle-get (item-id)
 (assert (not (null item-id)))
 (let* ((item (item-load item-id))
        (acl (cas-load (item-acl-ref item) +ACL-HIVE+))
        (acl-length (1- (length acl)))
        (user-id-accumulator (make-array +word-size+ :element-type '(unsigned-byte 8)))
        (email-list '()))
  (loop for i from 0 to acl-length by +word-size+
   do
   (memcpy user-id-accumulator acl 0 i +word-size+)
   (with-connection *db-connection-parameters*
    (let* ((user-id (word-bytes-to-fixnum user-id-accumulator))
           (rows (query (:select 'email :from 'users :where (:= 'user_id user-id))))
           (email (caar rows)))
     (if (not (null email))
         (push email email-list))))
  )
  (json:encode-json-to-string email-list)
 )
)

; send-new-password can be shared between the new user invite code and the 
; forgotten password code.
(defun send-new-password (email password)
 (cl-smtp:send-email "smtp.gmail.com"
                     "max@irsvp.cc"
                     email
                     "Your new password @kobbweb.net!"
                     (format nil "Your new password is:~%~%~a~%~%Pretty large and ungainly, huh? I'd recommend you change it soon!~%~%Cheers!~%-Max Burke~%kobbweb.net" password)
                     :ssl :tls
                     :authentication '(:login "max@irsvp.cc" "W0zixege")
 )
)

(defun invite-user (email item-id)
 (let* ((password (login-create-temporary-password email))
        (user-id (create-new-user email password)))
  ;; This is going to fail to compile. I need to think of what's going on here,
  ;; what I'd like to do. Specifically, I'll need email addresses for the site
  ;; itself, in addition to the email endpoints, and I'll have to separate them
  ;; somehow.
  ;;
  ;; TODO: Maybe extend the C program to forward onto the mail host. Maybe have 
  ;; a separate domain. Either way, things to think about.
  ;;
  ;; TODO: also change this so that it doesn't use your home email!
  (cl-smtp:send-email "smtp.gmail.com"
                      "max@irsvp.cc"
                      email
                      "You're invited to kobbweb!"
                      (format nil "Welcome to kobbweb!~%~%You have been invited to take part in a discussion at kobbweb! An account has been created for you and your new password will come in the next email you receive from us. After logging in, visit this link and take part in the conversation!~%~%http://www.kobbweb.net/x/~a~%~%Hope to see you soon!~%-Max Burke~%kobbweb.net" (byte-vector-to-hex-string item-id))
                      :ssl :tls
                      :authentication '(:login "max@irsvp.cc" "W0zixege"))
  (send-new-password email password)
  user-id
 )
)

(defun acl-resolve-user-id (email item-id)
 (with-connection *db-connection-parameters*
  (let* ((rows (query (:select 'user_id :from 'users :where (:= 'email email))))
         (user-id (caar rows)))
   (if (null user-id)
       (invite-user email item-id)
       user-id)
  )
 )
)

(defun acl-apply-add-to-item-and-children (item user-id)
 (let* ((acl-ref (item-acl-ref item))
        (acl (cas-load acl-ref +ACL-HIVE+))
        (new-acl (make-array (+ (length acl) +word-size+) :element-type '(unsigned-byte 8)))
        (user-id-bytes (fixnum-to-word-bytes user-id)))
  ;; Populate the ACL with the new user id
  (memcpy new-acl user-id-bytes 0 0 +word-size+)
  (memcpy new-acl acl +word-size+ 0 (length acl))
  
  ;; Push the new ACL to the CAS and update the item with the hash of the new ACL
  (let ((new-acl-ref (cas-store new-acl +ACL-HIVE+)))
   (setf (item-acl-ref item) new-acl-ref)
   (item-update item)
   (setf item (item-load (item-uuid item)))
   
   ;; Doublecheck that we haven't been beat here by some other request. Re-load
   ;; the item and check that the acl ref is what we just set above. If they 
   ;; differ then re-do the add, otherwise recurse into the children and continue
   ;; the process.
   (if (uuid= new-acl-ref (item-acl-ref item))
       (let* ((child-list-ref (item-list-ref item))
              (child-list (cas-load child-list-ref +LIST-HIVE+))
              (current-uuid (make-array +uuid-size+ :element-type '(unsigned-byte 8)))
              (child-list-length (1- (length child-list))))
        (loop for i from 0 to child-list-length by +uuid-size+
         do
         (memcpy current-uuid child-list 0 i +uuid-size+)
         (let ((child-item (item-load current-uuid)))
          (acl-apply-add-to-item-and-children child-item user-id))
        )
       )
       (acl-apply-add-to-item-and-children item user-id)
   )
  )
 )
)

(defun acl-handle-post (item-id)
 (assert (not (null item-id)))
 (with-posted-json (json)
  (let* ((item (item-load item-id))
         (email (cdr (assoc :email json)))
         (user-id (acl-resolve-user-id email item-id)))

  ;; TODO: The compiler thinks the call to acl-apply-add-to-item-and-children below is unreachable. Figure out why.
   (when (not (acl-is-member-of (item-acl-ref item) user-id))
         (acl-apply-add-to-item-and-children item user-id))

   *successful-post-response*
  )
 )
)

(defun acl-handler (uri)
 (let* ((req (request-method* *request*))
        (item-id-string (cadr uri))
        (item-id (if (not (null item-id-string))
                     (hex-string-to-byte-vector item-id-string)
                     nil)))
  (if (not (null item-id))
      (progn (cond ((eq req :get) (acl-handle-get item-id))
                   ((eq req :post) (acl-handle-post item-id))))
      (set (return-code*) +http-bad-request+))
 )
)
