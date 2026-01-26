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

;;; ==========================================================================
;;; Phase C: Structured Error Capture
;;; ==========================================================================

(defun capture-restarts (condition)
  "Capture available restarts for CONDITION as structured data."
  (loop for restart in (compute-restarts condition)
        collect (list :name (let ((name (restart-name restart)))
                              (when name (string name)))
                      :description (princ-to-string restart))))

(defun parse-backtrace-string (bt-string &optional (max-frames *max-backtrace-depth*))
  "Parse a backtrace string into structured frames.
Returns list of plists with :number, :function."
  (with-input-from-string (s bt-string)
    ;; Skip header line (Backtrace for: ...)
    (read-line s nil nil)
    (loop for line = (read-line s nil nil)
          for count from 0
          while (and line (< count max-frames))
          for parsed = (parse-backtrace-line line)
          when parsed collect parsed)))

(defun parse-backtrace-line (line)
  "Parse a single backtrace line like '0: (FUNCTION ARG1 ARG2)'.
Returns plist with :number and :function, or NIL if unparseable."
  (let ((colon-pos (position #\: line)))
    (when (and colon-pos (> colon-pos 0))
      (let ((num-str (string-trim " " (subseq line 0 colon-pos)))
            (rest (string-trim " " (subseq line (1+ colon-pos)))))
        (handler-case
            (list :number (parse-integer num-str)
                  :function rest)
          (error () nil))))))

(defun capture-structured-error (condition)
  "Capture comprehensive structured info about CONDITION.
Called at the point of error before unwinding.
Returns a plist with:
  :type - condition type name (string)
  :message - formatted condition message
  :condition-class - full class name
  :backtrace-string - raw backtrace as string
  :backtrace - parsed backtrace as list of frame plists
  :restarts - list of available restarts
  :timestamp - when the error occurred"
  (let ((bt-string (format-backtrace)))
    (list :type (condition-type-name condition)
          :message (condition-message condition)
          :condition-class (class-name (class-of condition))
          :backtrace-string bt-string
          :backtrace (parse-backtrace-string bt-string)
          :restarts (capture-restarts condition)
          :timestamp (get-universal-time))))

(defun format-structured-error (error-info)
  "Format structured error info for human-readable display."
  (with-output-to-string (s)
    (format s "[ERROR] ~A~%" (getf error-info :type))
    (format s "~A~%~%" (getf error-info :message))
    ;; Restarts section
    (let ((restarts (getf error-info :restarts)))
      (when restarts
        (format s "[Restarts]~%")
        (loop for r in restarts
              for i from 0
              do (format s "  ~D: [~A] ~A~%"
                         i
                         (or (getf r :name) "unnamed")
                         (getf r :description)))
        (terpri s)))
    ;; Backtrace section (structured)
    (let ((bt (getf error-info :backtrace)))
      (when bt
        (format s "[Backtrace]~%")
        (dolist (frame bt)
          (format s "~D: ~A~%"
                  (getf frame :number)
                  (getf frame :function)))))))

(defun format-backtrace-detail (error-info &key (max-frames *max-backtrace-depth*))
  "Format detailed backtrace from stored error info."
  (with-output-to-string (s)
    (let ((bt (getf error-info :backtrace)))
      (if bt
          (progn
            (format s "Backtrace (~D frame~:P):~%~%" (min (length bt) max-frames))
            (loop for frame in bt
                  for count from 0 below max-frames
                  do (format s "Frame ~D:~%  ~A~%~%"
                             (getf frame :number)
                             (getf frame :function))))
          (format s "No backtrace available.~%")))))
