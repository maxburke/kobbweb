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
    ((user_id :type integer)
     (item_id :type string)
     (alias :type string))
   )
  )

  ;;;;;
  ;;; Temporarily using Postgres as our key/value store, for easy hackability.
  (execute
   (:create-table lists
    ((key :type :bytea :primary-key t)
     (:value :type :bytea))
   )
  )
  (execute
   (:create-table acl
    ((key :type :bytea :primary-key t)
     (:value :type :bytea))
   )
  )
  (execute
   (:create-table data
    ((key :type :bytea :primary-key t)
     (:value :type :bytea))
   )
  )
  (execute
   (:create-table items
    ((key :type :bytea :primary-key t)
     (:value :type :bytea))
   )
  )
 )
)

