(in-package :kobbweb)

(defun handle-logged-in ()
)

(defun index-handler ()
 (if *session*
  (handle-logged-in)
  (handle-static-file #p"static/index.html")
 )
)

