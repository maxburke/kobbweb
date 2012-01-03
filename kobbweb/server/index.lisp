(in-package :kobbweb)

(defun index-handler ()
 (if *session*
  (redirect "/home")
  (handle-static-file #p"static/index.html")
 )
)

