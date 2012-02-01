(in-package :kobbweb)

(defun home-handler ()
 (if (null *session*)
     (redirect "/")
     (handle-static-file #p"static/home.html")
 )
)

