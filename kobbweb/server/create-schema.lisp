(in-package :kobbweb)
    
(defvar *db-connection-parameters* (list "kobbweb" "kobbweb" "kobbweb" "localhost" :pooled-p t))

(defun create-schema ()
 (with-connection *db-connection-parameters*
  (unless (table-exists-p 'users)
   (execute
    (:create-table users
     ((user_id :type serial :primary-key t)
      (email :type text)
      (password :type text))
    )
   )
  )

  (unless (table-exists-p 'posts)
   (execute
    (:create-table posts
     ((post_id :type serial :primary-key t)
      (user_id :type integer)
      (item_id :type string))
    )
   )
  )

  (unless (table-exists-p 'aliases)
   (execute
    (:create-table aliases
     ((user_id :type integer)
      (item_id :type string)
      (alias :type string))
    )
   )
  )
 
   ;;;;;
   ;;; Temporarily using Postgres as our key/value store, for easy hackability.
  (unless (table-exists-p 'lists)
   (execute
    (:create-table lists
     ((key :type :bytea :primary-key t)
      (:value :type :bytea))
    )
   )
  )
  (unless (table-exists-p 'acl)
   (execute
    (:create-table acl
     ((key :type :bytea :primary-key t)
      (:value :type :bytea))
    )
   )
  )
  (unless (table-exists-p 'data)
   (execute
    (:create-table data
     ((key :type :bytea :primary-key t)
      (:value :type :bytea))
    )
   )
  )
  (unless (table-exists-p 'items)
   (execute
    (:create-table items
     ((key :type :bytea :primary-key t)
      (:value :type :bytea))
    )
   )
  )

  (unless (table-exists-p 'invite_requests)
   (execute
    (:create-table invite_requests
     ((email :type string :primary-key t))
    )
   )
  )
 )
)

