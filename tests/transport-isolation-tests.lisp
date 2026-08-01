;;; tests/transport-isolation-tests.lisp
;;; ABOUTME: Subprocess tests that the JSON-RPC stream survives hostile user code

(in-package #:cl-mcp-server-tests)

(def-suite transport-isolation-tests
  :description "Protocol stream isolation against threads spawned by evaluated code"
  :in cl-mcp-server-tests)

(in-suite transport-isolation-tests)

;;; ==========================================================================
;;; Helpers
;;;
;;; These tests must run the real launcher in a real subprocess. The behaviour
;;; under test is process-global (SB-EXT:*INVOKE-DEBUGGER-HOOK*, the global
;;; value of *STANDARD-OUTPUT* seen by freshly spawned threads), so it cannot
;;; be exercised in-process without corrupting the test runner itself.
;;; ==========================================================================

(defun launcher-path ()
  "Absolute path to the run-server.lisp launcher."
  (namestring (asdf:system-relative-pathname "cl-mcp-server" "run-server.lisp")))

(defun run-server-subprocess (requests)
  "Feed REQUESTS (a list of JSON-RPC strings) to the launcher over stdin.
Returns (values stdout stderr exit-code) after stdin closes."
  (uiop:run-program (list "sbcl" "--script" (launcher-path))
                    :input (make-string-input-stream
                            (format nil "~{~a~%~}" requests))
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun init-request ()
  "The MCP initialize request."
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")

(defun eval-request (id code)
  "A tools/call request invoking evaluate-lisp on CODE."
  (with-output-to-string (s)
    (yason:encode
     (alexandria:alist-hash-table
      `(("jsonrpc" . "2.0")
        ("id" . ,id)
        ("method" . "tools/call")
        ("params" . ,(alexandria:alist-hash-table
                      `(("name" . "evaluate-lisp")
                        ("arguments" . ,(alexandria:alist-hash-table
                                         `(("code" . ,code))
                                         :test #'equal)))
                      :test #'equal)))
      :test #'equal)
     s)))

(defun protocol-lines (stdout)
  "Non-empty lines of STDOUT, which must all be JSON-RPC messages."
  (remove-if (lambda (s) (zerop (length (string-trim '(#\Space #\Return) s))))
             (split-by-newline stdout)))

(defun every-line-is-json-p (stdout)
  "True when every non-empty line of STDOUT parses as JSON."
  (every (lambda (line)
           (handler-case (progn (yason:parse line :object-as :alist) t)
             (error () nil)))
         (protocol-lines stdout)))

;;; ==========================================================================
;;; Regression tests for bug.md
;;; ==========================================================================

(test worker-thread-error-does-not-kill-server
  "An unhandled error in a thread spawned by evaluated code must abort only
that thread. The server must survive and answer subsequent requests.

Before the fix, --script implies --disable-debugger, whose hook quits the
whole process, so the server died mid-session and later requests went
unanswered."
  (multiple-value-bind (stdout stderr exit-code)
      (run-server-subprocess
       (list (init-request)
             (eval-request 2 "(progn (bordeaux-threads:make-thread (lambda () (error \"boom from worker\")) :name \"boomer\") (sleep 2) :spawned)")
             (eval-request 3 "(+ 1 2)")))
    (declare (ignore stderr))
    (is (= 0 exit-code)
        "Server must exit 0 on EOF, not be killed by the worker thread")
    (is (= 3 (length (protocol-lines stdout)))
        "All three requests must be answered")
    (is (search "=> 3" stdout)
        "The request after the worker error must still be served")))

(test worker-thread-output-does-not-corrupt-protocol
  "Output written to *standard-output* by a thread spawned by evaluated code
must never reach the JSON-RPC stream. Threads get the global binding, not the
evaluator's per-evaluation capture stream."
  (multiple-value-bind (stdout stderr exit-code)
      (run-server-subprocess
       (list (init-request)
             (eval-request 2 "(progn (bordeaux-threads:make-thread (lambda () (format t \"GARBAGE-ON-STDOUT~%\") (finish-output)) :name \"noisy\") (sleep 2) :spawned)")
             (eval-request 3 "(+ 1 2)")))
    (declare (ignore stderr))
    (is (= 0 exit-code))
    (is (null (search "GARBAGE-ON-STDOUT" stdout))
        "Worker thread output must not appear on the protocol stream")
    (is (every-line-is-json-p stdout)
        "Every line on stdout must be a valid JSON-RPC message")
    (is (= 3 (length (protocol-lines stdout))))))

;;; Note: code that writes directly to the file descriptor (SB-SYS:*STDOUT*)
;;; cannot be defended against by rebinding dynamic variables. Guarding that
;;; would require dup(2)-ing fd 1 for the protocol and reopening fd 1 onto
;;; stderr. Out of scope here; recorded so the gap is known rather than
;;; assumed covered.
