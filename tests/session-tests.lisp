;;; tests/session-tests.lisp
;;; ABOUTME: Tests for session management

(in-package #:cl-mcp-server-tests)

(def-suite session-tests
  :description "Session management tests"
  :in cl-mcp-server-tests)

(in-suite session-tests)

;;; Task 1: Basic session structure tests

(test make-session-default
  "Creating a session with default package"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (not (null session)))
    (is (eq (find-package :cl-user)
            (cl-mcp-server.session:session-package session)))
    (is (null (cl-mcp-server.session:session-loaded-systems session)))))

(test make-session-with-package-object
  "Creating a session with specific package object"
  (let ((session (cl-mcp-server.session:make-session
                  :package (find-package :cl))))
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test make-session-with-package-name
  "Creating a session with package name (symbol)"
  (let ((session (cl-mcp-server.session:make-session :package :cl)))
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test make-session-with-package-string
  "Creating a session with package name (string)"
  (let ((session (cl-mcp-server.session:make-session :package "CL")))
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test with-session-binds-special
  "with-session binds *session* during body"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (null cl-mcp-server.session:*session*))
    (cl-mcp-server.session:with-session (session)
      (is (eq session cl-mcp-server.session:*session*)))
    (is (null cl-mcp-server.session:*session*))))

(test with-session-returns-body-value
  "with-session returns the value of the body"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (= 42
           (cl-mcp-server.session:with-session (session)
             42)))))

(test session-definitions-initially-empty
  "session-definitions is initially empty"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (null (cl-mcp-server.session:session-definitions session)))))

;;; Task 2: Definition listing tests

(test list-definitions-empty
  "list-definitions returns empty list for fresh session"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (null (cl-mcp-server.session:list-definitions session)))))

(test list-definitions-with-data
  "list-definitions returns definitions from session"
  (let ((session (cl-mcp-server.session:make-session)))
    (setf (cl-mcp-server.session:session-definitions session)
          '((:function . test-fn)
            (:variable . *test-var*)
            (:macro . test-macro)))
    (let ((defs (cl-mcp-server.session:list-definitions session)))
      (is (= 3 (length defs)))
      (is (member '(:function . test-fn) defs :test #'equal)))))

(test list-definitions-by-type
  "list-definitions can filter by type"
  (let ((session (cl-mcp-server.session:make-session)))
    (setf (cl-mcp-server.session:session-definitions session)
          '((:function . test-fn1)
            (:function . test-fn2)
            (:variable . *test-var*)
            (:macro . test-macro)))
    (let ((funcs (cl-mcp-server.session:list-definitions session :type :function)))
      (is (= 2 (length funcs)))
      (is (every (lambda (d) (eq :function (car d))) funcs)))))

(test format-definitions-empty
  "format-definitions returns message for empty session"
  (let ((session (cl-mcp-server.session:make-session)))
    (let ((output (cl-mcp-server.session:format-definitions session)))
      (is (stringp output))
      (is (search "No definitions" output)))))

(test format-definitions-with-data
  "format-definitions returns formatted string"
  (let ((session (cl-mcp-server.session:make-session)))
    (setf (cl-mcp-server.session:session-definitions session)
          '((:function . my-function)
            (:variable . *my-var*)))
    (let ((output (cl-mcp-server.session:format-definitions session)))
      (is (stringp output))
      (is (search "my-function" output))
      (is (search "*my-var*" output)))))

;;; Task 3: Session reset tests

(test reset-session-clears-definitions
  "reset-session clears definitions"
  (let ((session (cl-mcp-server.session:make-session)))
    (setf (cl-mcp-server.session:session-definitions session)
          '((:function . test-fn)))
    (cl-mcp-server.session:reset-session session)
    (is (null (cl-mcp-server.session:session-definitions session)))))

(test reset-session-clears-loaded-systems
  "reset-session clears loaded systems"
  (let ((session (cl-mcp-server.session:make-session)))
    (setf (cl-mcp-server.session:session-loaded-systems session)
          '(:system-a :system-b))
    (cl-mcp-server.session:reset-session session)
    (is (null (cl-mcp-server.session:session-loaded-systems session)))))

(test reset-session-preserves-package
  "reset-session preserves current package"
  (let ((session (cl-mcp-server.session:make-session :package :cl)))
    (setf (cl-mcp-server.session:session-definitions session)
          '((:function . test-fn)))
    (cl-mcp-server.session:reset-session session)
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test reset-session-returns-session
  "reset-session returns the session for chaining"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (eq session (cl-mcp-server.session:reset-session session)))))

;;; Task 4: Package switching tests

(test switch-package-by-symbol
  "switch-package changes session package by symbol"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:switch-package session :cl)
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test switch-package-by-string
  "switch-package changes session package by string"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:switch-package session "COMMON-LISP")
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test switch-package-by-object
  "switch-package changes session package by package object"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:switch-package session (find-package :cl))
    (is (eq (find-package :cl)
            (cl-mcp-server.session:session-package session)))))

(test switch-package-returns-session
  "switch-package returns the session for chaining"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (eq session (cl-mcp-server.session:switch-package session :cl)))))

(test switch-package-signals-error-for-nonexistent
  "switch-package signals error for nonexistent package"
  (let ((session (cl-mcp-server.session:make-session)))
    (signals cl-mcp-server.conditions:invalid-params
      (cl-mcp-server.session:switch-package session :nonexistent-package-xyz))))
