;;; src/stdio-guard.lisp
;;; ABOUTME: Process-global guards keeping user code away from the JSON-RPC pipe

(in-package #:cl-mcp-server)

;;; The MCP transport is the process's real stdin/stdout. Anything else that
;;; touches those file descriptors corrupts the protocol. Two hazards are
;;; process-global, so neither can be fixed by the evaluator's per-evaluation
;;; LET bindings:
;;;
;;;   1. Threads spawned by evaluated code inherit the GLOBAL value of
;;;      *STANDARD-OUTPUT*, not the evaluator's capture stream. A stray
;;;      FORMAT T in a Hunchentoot worker lands mid-protocol.
;;;
;;;   2. SBCL's --script implies --disable-debugger, whose hook prints a
;;;      backtrace and QUITS THE PROCESS. One unhandled error in any thread
;;;      killed the whole server.
;;;
;;; Both are fixed with SETF rather than LET: dynamic bindings are
;;; thread-local, and threads the user spawns would never see them.

(defvar *log-lock* (sb-thread:make-mutex :name "cl-mcp-server-log")
  "Serialises crash reports. Several worker threads can fail at once, and
unsynchronised writes interleave mid-character into unreadable logs.")

(defun %report-unhandled (condition log-stream)
  "Log CONDITION to LOG-STREAM. Never signals; logging must not itself fail.

The message is rendered before the lock is taken: a condition's report
function is user code, so it may signal or even log, and neither should
happen while the lock is held."
  (ignore-errors
   (let ((message (format nil "~&[cl-mcp-server] unhandled ~a in thread ~a:~%  ~a~%"
                          (type-of condition)
                          (sb-thread:thread-name sb-thread:*current-thread*)
                          condition)))
     (sb-thread:with-mutex (*log-lock*)
       (write-string message log-stream)
       (finish-output log-stream)))))

(defun %make-debugger-hook (log-stream)
  "Build a hook that logs to LOG-STREAM instead of entering the debugger.

The hook must never return normally — returning re-enters the debugger. It
resumes via CONTINUE when the condition offers it (so user code calling BREAK
merely logs and carries on), otherwise unwinds just the offending thread."
  (lambda (condition hook)
    (declare (ignore hook))
    (%report-unhandled condition log-stream)
    (let ((continue-restart (find-restart 'continue condition)))
      (when continue-restart
        (invoke-restart continue-restart)))
    (if (sb-thread:main-thread-p)
        (sb-ext:exit :code 1 :abort t)
        (sb-thread:abort-thread))))

(defun %set-stream-everywhere (symbol value)
  "Point SYMBOL at VALUE in both the global value cell and the current binding.

Plain SETF is not enough. SBCL's --script establishes a dynamic binding of the
standard streams around the script, so SETF reaches only that binding while
freshly spawned threads keep reading the global cell — which still points at
the protocol pipe. Threads inherit the global value, never the parent's
binding, so the global cell is the one that actually matters here."
  (setf (sb-ext:symbol-global-value symbol) value)
  (setf (symbol-value symbol) value))

(defun install-process-guards (&key (log-stream *error-output*))
  "Divert every global stream variable away from the JSON-RPC pipe and stop
unhandled conditions from killing the process.

Call this AFTER capturing the real stdin/stdout for the transport. Returns no
useful value; the effects are process-global and deliberately permanent."
  (let ((quiet (make-two-way-stream (make-string-input-stream "") log-stream)))
    ;; Stray output goes to stderr, where the MCP client logs it for us.
    (%set-stream-everywhere '*standard-output* log-stream)
    (%set-stream-everywhere '*trace-output* log-stream)
    ;; Nothing may consume the protocol stream looking for input.
    (%set-stream-everywhere '*standard-input* (make-string-input-stream ""))
    ;; *debug-io* and *query-io* are synonyms of *terminal-io* by default;
    ;; set all three so no path back to the tty survives.
    (%set-stream-everywhere '*terminal-io* quiet)
    (%set-stream-everywhere '*debug-io* quiet)
    (%set-stream-everywhere '*query-io* quiet))
  (setf sb-ext:*invoke-debugger-hook* (%make-debugger-hook log-stream))
  (values))
