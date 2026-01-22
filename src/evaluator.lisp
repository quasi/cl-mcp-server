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
warnings encountered, and error information."
  (success-p nil :type boolean)
  (values nil :type list)
  (stdout "" :type string)
  (stderr "" :type string)
  (warnings nil :type list)
  (error nil))

(defun make-evaluation-result (&key success-p values stdout stderr warnings error-info)
  "Create an evaluation result with proper defaults.
SUCCESS-P indicates whether evaluation completed without errors.
VALUES is a list of string representations of return values.
STDOUT and STDERR capture printed output during evaluation.
WARNINGS is a list of formatted warning strings.
ERROR-INFO contains formatted error information when evaluation fails."
  (%make-evaluation-result
   :success-p success-p
   :values values
   :stdout (or stdout "")
   :stderr (or stderr "")
   :warnings (or warnings nil)
   :error (or error-info nil)))
