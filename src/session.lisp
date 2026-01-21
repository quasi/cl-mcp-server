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
