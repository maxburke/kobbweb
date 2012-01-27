(in-package :kobbweb)

; Like /home, /x is a static page and all the dynamic content is handled by JS.
(defun x-handler ()
 (handle-static-file #p"static/x.html")
)

