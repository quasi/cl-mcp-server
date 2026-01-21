;;; src/error-format.lisp
;;; ABOUTME: Condition formatting for MCP error responses

(in-package #:cl-mcp-server.error-format)

;;; Configuration

(defparameter *max-backtrace-depth* 20
  "Maximum number of backtrace frames to include")

(defparameter *print-backtrace-p* t
  "Whether to include backtrace in error output")

;;; Condition type extraction

(defun condition-type-name (condition)
  "Get the type name of a condition as a string"
  (string-upcase (symbol-name (type-of condition))))

;;; Message extraction

(defun condition-message (condition)
  "Extract the message from a condition"
  (handler-case
      (princ-to-string condition)
    (error ()
      (format nil "~a" (type-of condition)))))

;;; Backtrace formatting

(defun format-backtrace ()
  "Capture and format the current backtrace.
   Returns a string with numbered frames."
  (with-output-to-string (s)
    (trivial-backtrace:print-backtrace-to-stream s)))

(defun truncate-backtrace (backtrace-string)
  "Truncate backtrace to *max-backtrace-depth* frames"
  (with-input-from-string (in backtrace-string)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil)
            for count from 0
            while (and line (< count *max-backtrace-depth*))
            do (write-line line out)
            finally (when line
                      (write-line "..." out))))))

;;; Main formatting functions

(defun format-error (condition)
  "Format an error condition for MCP response."
  (with-output-to-string (s)
    (format s "[ERROR] ~a~%" (condition-type-name condition))
    (format s "~a~%" (condition-message condition))
    (when *print-backtrace-p*
      (format s "~%[Backtrace]~%")
      (write-string (truncate-backtrace (format-backtrace)) s))))

(defun format-warning (condition)
  "Format a warning condition."
  (format nil "~a: ~a"
          (condition-type-name condition)
          (condition-message condition)))

(defun format-condition (condition)
  "Format any condition appropriately"
  (if (typep condition 'warning)
      (format-warning condition)
      (format-error condition)))

;;; Error capture macro

(defmacro with-error-capture (&body body)
  "Execute BODY, capturing any errors and warnings.
Returns three values:
  1. List of return values from BODY (or NIL on error)
  2. Formatted error string (or NIL on success)
  3. List of formatted warning strings"
  (let ((warnings (gensym "WARNINGS"))
        (error-string (gensym "ERROR-STRING"))
        (results (gensym "RESULTS")))
    `(let ((,warnings nil)
           (,error-string nil)
           (,results nil))
       (handler-bind
           ((warning (lambda (c)
                       (push (format-warning c) ,warnings)
                       (muffle-warning c))))
         (handler-case
             (setf ,results (multiple-value-list (progn ,@body)))
           (error (c)
             (setf ,error-string (format-error c)))))
       (values ,results ,error-string (nreverse ,warnings)))))
