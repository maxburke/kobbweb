(in-package :kobbweb)

(defmacro variable-dispatcher (content-type content)
  `(lambda ()
    (setf (content-type*) ,content-type)
    ,content))

(defun split-uri-by-slashes (uri)
 (let ((collection '()))
  (labels ((recursive-extract (uri-fragment)
            (if uri-fragment
             (let ((fragment-end (position #\/ uri-fragment)))
              (push (subseq uri-fragment 0 fragment-end) collection)
              (if fragment-end
               (recursive-extract (subseq uri-fragment (1+ fragment-end))))))))
  (recursive-extract (subseq uri (1+ (position #\/ uri)))))
  (nreverse collection)
 )
)

(defmacro uri-dispatcher (fn)
 `(lambda ()
     (let ((uri-components (split-uri-by-slashes (request-uri* *request*))))
      (funcall ,fn uri-components))
  )
)
    
(defun init ()
 (if (not (null *server-instance*))
  (stop *server-instance*))

 (setf *server-instance* 
  (make-instance 'acceptor :port 8000))

 (start *server-instance*)

 (setf *dispatch-table*
  (nconc (list 'dispatch-easy-handlers
          (create-folder-dispatcher-and-handler "/static/" #p"static/")
          (create-prefix-dispatcher "/sessions" #'sessions-handler)
          (create-prefix-dispatcher "/content" (uri-dispatcher #'content-handler))
          (create-prefix-dispatcher "/home" #'home-handler)
          (create-prefix-dispatcher "/data" (uri-dispatcher #'data-handler))
          (create-prefix-dispatcher "/posts" (uri-dispatcher #'posts-handler))
          (create-prefix-dispatcher "/email" #'email-handler)
          (create-prefix-dispatcher "/x" #'x-handler)
          (create-prefix-dispatcher "/" #'index-handler)
          ; TODO:
          ; Add special pages (ie, /500, /404) that have custom error messages for certain conditions.
          #'default-dispatcher))))

