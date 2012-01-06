(in-package :kobbweb)

(defun home-handler ()
 (handle-static-file #p"static/home.html")
)

