(in-package :kobbweb)

(defvar *server-instance* nil)
(defvar *kobbweb-index* #p"/home/max/src/lisp/kobbweb/index.html")
(defvar *db-connection-parameters* (list "kobbweb" "kobbweb" "kobbweb" "localhost" :pooled-p t))

;; Encode the user agent into the session secret so that a new session is required if this changes.
(setf *use-user-agent-for-sessions* t)
;; Encode the remote address into the session secret so that a new session is required if this changes.
(setf *use-remote-addr-for-sessions* t)
;; Set the session max time to be 7 days.
(setf *session-max-time* 604800)
; Permit post-parameters to be retrieved from PUT and DELETE methods, as well as POST
(nconc *methods-for-post-parameters* '(:put :delete))

; These three should be *off* for release.
(setf *show-lisp-errors-p* t)
(setf *message-log-pathname* #p"/Users/max/src/lisp/kobbweb/logs/message.log")
(setf *access-log-pathname* #p"/Users/max/src/lisp/kobbweb/logs/access.log")
(defvar *kobbweb-debug* t)

(if *kobbweb-debug*
 (defmacro server-log (str)
  `(log-message *lisp-warnings-log-level* ,str))
 (defmacro server-log (str)))

; Debugging facilities
(defvar *kobbweb-debug* t)

(if *kobbweb-debug*
 (defmacro server-log (str)
  `(log-message *lisp-warnings-log-level* ,str))
 (defmacro server-log (str)))

; Tell Hunchentoot to not handle 404 specially. This lets the server application return
; a "not found" status code if certain records aren't found, without sending back error
; text.
(nconc *approved-return-codes* (list +http-not-found+ +http-forbidden+ +http-bad-request+))

; *successful-post-response* is a generic JSON response for when something is returned to
; a client. It is equivalent to { "success" : "true" }. "true"/"false" is currently used,
; as opposed to the boolean true/false values, because cl-json cannot serialize false, 
; only null.
(defvar *successful-post-response* (json:encode-json-to-string '((:success . "true"))))

