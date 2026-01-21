;;; src/session.lisp
;;; ABOUTME: Session state management for persistent REPL

(in-package #:cl-mcp-server.session)

;;; Session state

(defvar *session* nil
  "The current session, bound during evaluation")

(defstruct (session (:constructor %make-session))
  "A REPL session with persistent state"
  (package (find-package :cl-user) :type package)
  (loaded-systems nil :type list)
  (definitions nil :type list))

(defun make-session (&key (package (find-package :cl-user)))
  "Create a new session with optional package context.
PACKAGE can be a package object, symbol, or string."
  (%make-session :package (etypecase package
                            (package package)
                            ((or symbol string) (find-package package)))))

(defmacro with-session ((session) &body body)
  "Execute BODY with *session* bound to SESSION.
Returns the value of the last form in BODY."
  `(let ((*session* ,session))
     ,@body))

;;; Definition listing

(defun list-definitions (session &key type)
  "List definitions tracked in SESSION.
If TYPE is specified (e.g., :function, :variable, :macro), filter by that type.
Returns a list of (type . name) pairs."
  (let ((defs (session-definitions session)))
    (if type
        (remove-if-not (lambda (def) (eq type (car def))) defs)
        defs)))

(defun format-definitions (session &key type)
  "Format session definitions as a human-readable string.
If TYPE is specified, only show definitions of that type."
  (let ((defs (list-definitions session :type type)))
    (if (null defs)
        "No definitions in session."
        (with-output-to-string (s)
          (format s "Session definitions:~%")
          (dolist (def defs)
            (format s "  ~a: ~a~%"
                    (string-downcase (symbol-name (car def)))
                    (string-downcase (symbol-name (cdr def)))))))))

;;; Session reset

(defun reset-session (session)
  "Reset SESSION to a fresh state.
Clears definitions and loaded systems, but preserves the current package.
Returns the session for chaining."
  (setf (session-definitions session) nil)
  (setf (session-loaded-systems session) nil)
  session)
