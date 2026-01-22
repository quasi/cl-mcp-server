;;; src/evaluator.lisp
;;; ABOUTME: Common Lisp code evaluation with output capture

(in-package #:cl-mcp-server.evaluator)

;;; ==========================================================================
;;; Evaluation Result Structure
;;; ==========================================================================

(defstruct (evaluation-result (:conc-name result-)
                              (:constructor %make-evaluation-result))
  "Result of evaluating Lisp code.
Contains success status, return values, captured output streams,
warnings encountered, error information, and definitions made."
  (success-p nil :type boolean)
  (values nil :type list)
  (stdout "" :type string)
  (stderr "" :type string)
  (warnings nil :type list)
  (error nil)
  (definitions nil :type list))

(defun make-evaluation-result (&key success-p values stdout stderr warnings error-info definitions)
  "Create an evaluation result with proper defaults.
SUCCESS-P indicates whether evaluation completed without errors.
VALUES is a list of string representations of return values.
STDOUT and STDERR capture printed output during evaluation.
WARNINGS is a list of formatted warning strings.
ERROR-INFO contains formatted error information when evaluation fails.
DEFINITIONS is a list of (type . name) pairs for definitions made."
  (%make-evaluation-result
   :success-p success-p
   :values values
   :stdout (or stdout "")
   :stderr (or stderr "")
   :warnings (or warnings nil)
   :error (or error-info nil)
   :definitions (or definitions nil)))

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

(defun evaluate-code (code-string)
  "Evaluate Common Lisp code from CODE-STRING.
Returns an EVALUATION-RESULT containing:
- Success status
- Return values (as formatted strings)
- Captured stdout output
- Captured stderr output
- Any warnings encountered
- Error information (if evaluation failed)
- Definitions made during evaluation

All forms in CODE-STRING are read and evaluated in sequence.
Only the values from the last form are returned."
  (let ((stdout-capture (make-string-output-stream))
        (stderr-capture (make-string-output-stream))
        (warnings-list nil)
        (result-values nil)
        (error-info nil)
        (success-p nil)
        (definitions nil))
    (handler-bind
        ((warning (lambda (c)
                    (push (format-warning c) warnings-list)
                    (muffle-warning c))))
      (handler-case
          (let ((*standard-output* stdout-capture)
                (*error-output* stderr-capture)
                (*trace-output* stderr-capture))
            (let ((forms (read-all-forms code-string)))
              ;; Extract definitions before evaluation
              (setf definitions (extract-definitions forms))
              (setf result-values (evaluate-forms forms))
              (setf success-p t)))
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
     :definitions (when success-p definitions))))

;;; ==========================================================================
;;; Result Formatting for MCP
;;; ==========================================================================

(defun format-result (result)
  "Format an EVALUATION-RESULT as a human-readable string for MCP.
Includes stdout output, warnings, return values, and errors."
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
        (let ((values (result-values result)))
          (if values
              (dolist (val values)
                (format s "=> ~a~%" val))
              (format s "; No values~%")))
        (write-string (result-error result) s))))
