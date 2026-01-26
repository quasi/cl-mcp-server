;;; src/evaluator.lisp
;;; ABOUTME: Common Lisp code evaluation with output capture and timeout protection

(in-package #:cl-mcp-server.evaluator)

;;; ==========================================================================
;;; Timeout Configuration
;;; ==========================================================================

(defparameter *evaluation-timeout* 30
  "Default timeout for code evaluation in seconds.
Set to NIL to disable timeout (not recommended for untrusted code).")

(defparameter *max-output-chars* 100000
  "Maximum characters to capture from stdout/stderr before truncation.")

;;; ==========================================================================
;;; Evaluation Result Structure
;;; ==========================================================================

(defstruct (evaluation-result (:conc-name result-)
                              (:constructor %make-evaluation-result))
  "Result of evaluating Lisp code.
Contains success status, return values, captured output streams,
warnings encountered, error information, definitions made, and timing info."
  (success-p nil :type boolean)
  (values nil :type list)
  (stdout "" :type string)
  (stderr "" :type string)
  (warnings nil :type list)
  (error nil)
  (definitions nil :type list)
  (timing nil :type list)  ; plist with :real-ms :run-ms :gc-ms :bytes-consed
  (package nil :type (or null string)))

(defun make-evaluation-result (&key success-p values stdout stderr warnings error-info definitions timing package)
  "Create an evaluation result with proper defaults.
SUCCESS-P indicates whether evaluation completed without errors.
VALUES is a list of string representations of return values.
STDOUT and STDERR capture printed output during evaluation.
WARNINGS is a list of formatted warning strings.
ERROR-INFO contains formatted error information when evaluation fails.
DEFINITIONS is a list of (type . name) pairs for definitions made.
TIMING is a plist with :real-ms :run-ms :gc-ms :bytes-consed.
PACKAGE is the package name where evaluation occurred."
  (%make-evaluation-result
   :success-p success-p
   :values values
   :stdout (or stdout "")
   :stderr (or stderr "")
   :warnings (or warnings nil)
   :error (or error-info nil)
   :definitions (or definitions nil)
   :timing timing
   :package package))

;;; ==========================================================================
;;; Value Formatting
;;; ==========================================================================

(defun format-value (value)
  "Format a single Lisp value as a readable string.
Uses *PRINT-PRETTY* for nice output, but limits length/depth
to avoid runaway printing."
  (let ((*print-pretty* t)
        (*print-length* 100)
        (*print-level* 10)
        (*print-circle* t))
    (prin1-to-string value)))

(defun format-values (values)
  "Format a list of values as strings.
Returns a list of string representations."
  (mapcar #'format-value values))

;;; ==========================================================================
;;; Definition Detection
;;; ==========================================================================

(defvar *definition-forms*
  '((defun . :function)
    (defmacro . :macro)
    (defvar . :variable)
    (defparameter . :variable)
    (defconstant . :variable)
    (defclass . :class)
    (defstruct . :struct)
    (defgeneric . :function)
    (defmethod . :method))
  "Mapping of definition forms to their types.")

(defun extract-definition (form)
  "Extract definition info from FORM if it's a definition.
Returns (type . name) or NIL if not a definition."
  (when (and (consp form)
             (symbolp (car form)))
    (let ((def-type (cdr (assoc (car form) *definition-forms*))))
      (when (and def-type (cdr form))
        (let ((name (cadr form)))
          ;; Handle defmethod's name extraction (might have qualifiers)
          (when (eq def-type :method)
            (setf name (if (listp name) (car name) name)))
          ;; Handle defstruct's name (might be (name options...))
          (when (and (eq def-type :struct) (listp name))
            (setf name (car name)))
          (when (symbolp name)
            (cons def-type name)))))))

(defun extract-definitions (forms)
  "Extract all definitions from a list of FORMS.
Returns a list of (type . name) pairs."
  (loop for form in forms
        for def = (extract-definition form)
        when def collect def))

;;; ==========================================================================
;;; Timing Support
;;; ==========================================================================

(defmacro with-timing (&body body)
  "Execute BODY and return (values result-values timing-plist).
TIMING-PLIST contains :real-ms :run-ms :gc-ms :bytes-consed."
  (let ((start-real (gensym "START-REAL"))
        (start-run (gensym "START-RUN"))
        (start-gc (gensym "START-GC"))
        (start-bytes (gensym "START-BYTES"))
        (result (gensym "RESULT")))
    `(let ((,start-real (get-internal-real-time))
           (,start-run (get-internal-run-time))
           (,start-gc sb-ext:*gc-run-time*)
           (,start-bytes (sb-ext:get-bytes-consed)))
       (let ((,result (multiple-value-list (progn ,@body))))
         (values ,result
                 (list :real-ms (round (* 1000 (- (get-internal-real-time) ,start-real))
                                       internal-time-units-per-second)
                       :run-ms (round (* 1000 (- (get-internal-run-time) ,start-run))
                                      internal-time-units-per-second)
                       :gc-ms (round (* 1000 (- sb-ext:*gc-run-time* ,start-gc))
                                     internal-time-units-per-second)
                       :bytes-consed (- (sb-ext:get-bytes-consed) ,start-bytes)))))))

;;; ==========================================================================
;;; Code Reading
;;; ==========================================================================

(defun read-all-forms (string)
  "Read all Lisp forms from STRING.
Returns a list of forms in order of appearance.
Signals reader errors for malformed input."
  (with-input-from-string (stream string)
    (loop with eof = (gensym "EOF")
          for form = (read stream nil eof)
          until (eq form eof)
          collect form)))

;;; ==========================================================================
;;; Code Evaluation
;;; ==========================================================================

(defun evaluate-forms (forms)
  "Evaluate a list of FORMS in sequence.
Returns multiple values from the last form."
  (if (null forms)
      (values)
      (loop for (form . rest) on forms
            if rest
              do (eval form)
            else
              return (multiple-value-list (eval form)))))

(defun capture-backtrace ()
  "Capture current backtrace as a string for timeout reporting."
  (with-output-to-string (s)
    (trivial-backtrace:print-backtrace-to-stream s)))

(defun %evaluate-with-timeout (thunk timeout)
  "Execute THUNK with TIMEOUT seconds limit.
Signals EVALUATION-TIMEOUT if the limit is exceeded.
Provides restarts: ABORT (return nil), USE-VALUE (return specified value)."
  (if (null timeout)
      ;; No timeout - just run directly
      (funcall thunk)
      ;; With timeout - use bordeaux-threads
      (let ((result-values nil)
            (error-occurred nil)
            (completed nil))
        (bt:with-timeout (timeout)
          (handler-case
              (progn
                (setf result-values (multiple-value-list (funcall thunk)))
                (setf completed t))
            (bt:timeout ()
              (setf error-occurred t)
              (restart-case
                  (error 'evaluation-timeout
                         :timeout-seconds timeout
                         :backtrace (capture-backtrace)
                         :message (format nil "Evaluation exceeded ~A second~:P" timeout))
                (abort ()
                  :report "Abort evaluation and return nil"
                  (setf result-values (list nil)))
                (use-value (value)
                  :report "Return a specified value instead"
                  :interactive (lambda () (list (read)))
                  (setf result-values (list value)))))))
        (values-list result-values))))

(defun evaluate-code (code-string &key (timeout *evaluation-timeout*) package (capture-time nil))
  "Evaluate Common Lisp code from CODE-STRING with timeout protection.
Returns an EVALUATION-RESULT containing:
- Success status
- Return values (as formatted strings)
- Captured stdout output
- Captured stderr output
- Any warnings encountered
- Error information (if evaluation failed)
- Definitions made during evaluation
- Timing information (if CAPTURE-TIME is true)
- Package name where evaluation occurred

TIMEOUT specifies maximum seconds for evaluation (default: *evaluation-timeout*).
Set to NIL to disable timeout.
PACKAGE specifies the package context (string or package, default: CL-USER).
CAPTURE-TIME if true, captures timing information.

All forms in CODE-STRING are read and evaluated in sequence.
Only the values from the last form are returned."
  (let* ((stdout-capture (make-string-output-stream))
         (stderr-capture (make-string-output-stream))
         (warnings-list nil)
         (result-values nil)
         (error-info nil)
         (success-p nil)
         (definitions nil)
         (timing-info nil)
         (eval-package (etypecase package
                         (null (find-package "CL-USER"))
                         (string (or (find-package (string-upcase package))
                                     (error "Package ~A not found" package)))
                         (package package)
                         (symbol (or (find-package package)
                                     (error "Package ~A not found" package))))))
    (handler-bind
        ((warning (lambda (c)
                    (push (format-warning c) warnings-list)
                    (muffle-warning c))))
      (handler-case
          (let ((*standard-output* stdout-capture)
                (*error-output* stderr-capture)
                (*trace-output* stderr-capture)
                (*package* eval-package))
            (let ((forms (read-all-forms code-string)))
              ;; Extract definitions before evaluation
              (setf definitions (extract-definitions forms))
              ;; Evaluate with or without timing
              (if capture-time
                  (multiple-value-bind (values timing)
                      (with-timing
                        (%evaluate-with-timeout
                         (lambda () (evaluate-forms forms))
                         timeout))
                    (setf result-values (car values))
                    (setf timing-info timing))
                  (setf result-values
                        (%evaluate-with-timeout
                         (lambda () (evaluate-forms forms))
                         timeout)))
              (setf success-p t)))
        (evaluation-timeout (c)
          (setf error-info (format-timeout-error c))
          (setf success-p nil))
        (error (c)
          (setf error-info (format-error c))
          (setf success-p nil))))
    (make-evaluation-result
     :success-p success-p
     :values (when success-p (format-values result-values))
     :stdout (get-output-stream-string stdout-capture)
     :stderr (get-output-stream-string stderr-capture)
     :warnings (nreverse warnings-list)
     :error-info error-info
     :definitions (when success-p definitions)
     :timing timing-info
     :package (package-name eval-package))))

(defun format-timeout-error (condition)
  "Format an EVALUATION-TIMEOUT condition for MCP output."
  (format nil "[TIMEOUT] Evaluation exceeded ~A second~:P~%~
               ~%Hint: The code may contain an infinite loop or expensive computation.~
               ~%Consider breaking into smaller operations or using configure-limits ~
               to increase timeout.~
               ~@[~%~%Backtrace at timeout:~%~A~]"
          (timeout-seconds condition)
          (timeout-backtrace condition)))

;;; ==========================================================================
;;; Result Formatting for MCP
;;; ==========================================================================

(defun format-timing-result (result)
  "Format an EVALUATION-RESULT with focus on timing information.
Used by time-execution tool to provide detailed profiling output."
  (with-output-to-string (s)
    (if (result-success-p result)
        (let ((timing (result-timing result))
              (values (result-values result)))
          ;; Timing info first (the main output)
          (if timing
              (progn
                (format s "Timing:~%")
                (format s "  Real time:      ~D ms~%" (getf timing :real-ms))
                (format s "  Run time:       ~D ms~%" (getf timing :run-ms))
                (format s "  GC time:        ~D ms~%" (getf timing :gc-ms))
                (format s "  Bytes consed:   ~:D~%" (getf timing :bytes-consed))
                (terpri s))
              (format s "Warning: No timing data captured~%~%"))
          ;; Then result values
          (format s "Result:~%")
          (if values
              (dolist (val values)
                (format s "  ~A~%" val))
              (format s "  ; No values~%"))
          ;; Stdout if any
          (let ((stdout (result-stdout result)))
            (unless (string= "" stdout)
              (format s "~%Output:~%~A" stdout))))
        ;; Error case
        (format s "Error during execution:~%~A" (result-error result)))))

(defun format-result (result)
  "Format an EVALUATION-RESULT as a human-readable string for MCP.
Includes stdout output, warnings, return values, timing, and errors."
  (with-output-to-string (s)
    ;; Output section
    (let ((stdout (result-stdout result)))
      (unless (string= "" stdout)
        (write-string stdout s)
        (unless (char= #\Newline (char stdout (1- (length stdout))))
          (terpri s))))
    ;; Stderr section (if any)
    (let ((stderr (result-stderr result)))
      (unless (string= "" stderr)
        (format s "[stderr] ~a~%" stderr)))
    ;; Warnings section
    (dolist (warning (result-warnings result))
      (format s "[Warning] ~a~%" warning))
    ;; Values or error
    (if (result-success-p result)
        (progn
          (let ((values (result-values result)))
            (if values
                (dolist (val values)
                  (format s "=> ~a~%" val))
                (format s "; No values~%")))
          ;; Timing section (if present)
          (let ((timing (result-timing result)))
            (when timing
              (format s "~%; Timing: ~Dms real, ~Dms run, ~Dms GC, ~:D bytes consed~%"
                      (getf timing :real-ms)
                      (getf timing :run-ms)
                      (getf timing :gc-ms)
                      (getf timing :bytes-consed)))))
        (write-string (result-error result) s))))
