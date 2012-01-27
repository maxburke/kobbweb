(in-package :kobbweb)

; / either redirects to /home if the user has logged in or the static page at
; /static/index.html if they have not.
(defun index-handler ()
 (if *session*
  (redirect "/home")
  (handle-static-file #p"static/index.html")
 )
)

