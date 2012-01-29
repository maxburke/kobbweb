(in-package :kobbweb)
    
(defvar *db-connection-parameters* (list "kobbweb" "kobbweb" "kobbweb" "localhost" :pooled-p t))

(defun create-schema ()
 (with-connection *db-connection-parameters*
  (execute
   (:create-table users
    ((user_id :type serial :primary-key t)
     (email :type text)
     (password :type text))
   )
  )
  (execute
   (:create-table posts
    ((post_id :type serial :primary-key t)
     (user_id :type integer)
     (item_id :type string))
   )
  )
  (execute
   (:create-table aliases
    ((user_id :type integer :primary-key t)
     (item_id :type string)
     (alias :type string))
   )
  )
 )
)

