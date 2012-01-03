(defpackage #:kobbweb-system
 (:use :cl :asdf))

(in-package :asdf)

(defsystem "kobbweb"
 :serial t
 :depends-on (:hunchentoot :cl-who :postmodern :cl-json)
 :components (
   (:module :server
    :serial t
    :components (
       (:static-file "kobbweb.asd")
       (:file "package")
       (:file "parameters")
       (:file "html-helpers")
       (:file "session")
       (:file "bad-passwords")
       (:file "init")
       (:file "create-schema")
      )
    )
  )
)


