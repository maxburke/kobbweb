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
;   (:create-table locations
;    ((location_id :type serial :primary-key t)
;     (name :type text)
;     (latitude :type double-float)
;     (longitude :type double-float))
;   )
;   (:create-table friends
;    ((user_id :type integer :references (users :no-action :no-action))
;     (friend_id :type integer :references (users :no-action :no-action))
;     (relationship :type integer :default 0))
;   )
;   (:create-table dive_log_entry
;    ((dive_log_id :type serial :primary-key t)
;     (user_id :type integer :references (users :no-action :no-action))
;     (location_id :type integer :references (locations :no-action :no-action))
;     (date :type timestamp)
;     (duration :type integer)
;     (depth :type integer)
;     (partner :type integer :references (users :no-action :no-action))
;     (notes :type text))
;   )
 )
)

