;;;; Contains helpers for handling html page generation (like generating headers and footers)

(in-package :kobbweb)

(defun kobbweb-prologue (html-stream page-title)
 (with-html-output (html-stream)
  (htm 
   (:head (:title (str page-title))
   )
  )
 )
)

(defmacro with-header ((html-stream) &body body)
 `(with-html-output (,html-stream)
    (htm
     (:head (:title "iRSVP")
      (:link :rel "stylesheet" :href "/static/bootstrap.min.css" :media "all" :rel "stylesheet" :type "text/css")
      (:link :rel "stylesheet" :href "/static/kobbweb.css" :media "all" :rel "stylesheet" :type "text/css")
     )
     (:body
      (:div :class "topbar"
       (:div :class "fill"
        (:div :class "container"
         (:ul :class "nav"
          (:li (:a :href "/" (:img :src "/static/icons/kobbweb-72x20.png")))
          (:li (:a :href "/contact" "Contact Us"))
          (if *session*
           (progn
            (htm
             (:li (:a :href "/logout" "Log Out"))
            )
           )
           (progn 
            (htm 
             (:li (:a :href "/join" "Sign Up"))
             (:li (:a :href "/login" "Log In"))
            )
           )
          )
         )
        )
       )
       )
     ,@body)
    )
  )
)

(defmacro with-header-no-login ((html-stream) &body body)
 `(with-html-output (,html-stream)
     (:div :id "title-bar"
      (:span :id "title-logo" "iRSVP")
      ,@body)))

(defun header (html-stream)
 (with-header-no-login (html-stream)
  (if *session*
   ;; Need to work on the "if-user-already-is-logged-in bit". should the page present
   ;; logout/welcome message on separate lines? probably not.
   (htm (:span :id "title-username"
         (format html-stream "Welcome ~a! [<a href=\"/logout\">logout</a>]" (session-value 'id *session*))))
   (htm (:span :id "title-userinput" 
         (:form :action "login" :method "post"
          (:input :type "text" :name "email" :id "title-email")
          (:input :type "password" :name "password" :id "title-password")
          (:input :type "submit" :value "Login" :id "title-loginbutton")))
    (:span :id "title-register"
     (:a :href "/register" "Create an account"))))))

(defun footer (html-stream)
 (with-html-output (html-stream)
  (:div :id "footer"
   (:p "footer"))))

(defmacro with-header-and-footer ((title) &body body)
 (let ((var (gensym)))
  `(with-html-output-to-string (,var)
      (htm
       (:html 
        (kobbweb-prologue ,var ,title)
        (:body (header ,var)
         ,@body
         (footer ,var)))))))

(defmacro defpage (name title &body body)
 `(defun ,name ()
     (with-header-and-footer (,title)
      ,@body)))

(defun html-404-handler ()
 (setf (return-code*) +http-not-found+)
 "Lost? Can we help you find your way back? (TODO: 404 handler!)"
)
