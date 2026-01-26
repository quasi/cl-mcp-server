;;; src/profiling-tools.lisp
;;; ABOUTME: Phase F - Profiling tools for performance analysis

(in-package #:cl-mcp-server.profiling-tools)

;;; Ensure sb-sprof is loaded at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

;;; ==========================================================================
;;; Statistical Profiling (sb-sprof)
;;; ==========================================================================

(defun introspect-profile-code (code &key (mode :cpu) (max-samples 1000)
                                       (sample-interval 0.01) (report-type :flat)
                                       (package "CL-USER"))
  "Profile CODE using statistical sampling.
MODE can be :CPU, :TIME, or :ALLOC.
Returns a plist with :result, :report, and profiling metadata."
  (let* ((pkg (or (find-package (string-upcase package))
                  (error "Package ~A not found" package)))
         (*package* pkg)
         (report-string nil)
         (result-values nil)
         (error-message nil))
    ;; Parse the code
    (let ((forms (with-input-from-string (s code)
                   (loop for form = (read s nil 'eof)
                         until (eq form 'eof)
                         collect form))))
      ;; Run with profiling
      (sb-sprof:reset)
      (handler-case
          (sb-sprof:with-profiling (:max-samples max-samples
                                    :mode mode
                                    :sample-interval sample-interval
                                    :reset t
                                    :report nil)
            (dolist (form forms)
              (setf result-values (multiple-value-list (eval form)))))
        (error (e)
          (setf error-message (format nil "~A" e))))
      ;; Capture report
      (setf report-string
            (with-output-to-string (*standard-output*)
              (sb-sprof:report :type report-type :max 50))))
    (list :success (null error-message)
          :result (if error-message
                      error-message
                      (format nil "~{~S~^~%~}" result-values))
          :mode mode
          :max-samples max-samples
          :sample-interval sample-interval
          :report report-string)))

(defun format-profile-code-result (info)
  "Format statistical profiling result as human-readable string."
  (with-output-to-string (s)
    (if (getf info :success)
        (progn
          (format s "Profiling completed (mode: ~A)~%~%" (getf info :mode))
          (format s "Result: ~A~%~%" (getf info :result))
          (format s "~A" (getf info :report)))
        (format s "Profiling failed: ~A~%~%~A"
                (getf info :result)
                (getf info :report)))))

;;; ==========================================================================
;;; Deterministic Profiling (sb-profile)
;;; ==========================================================================

(defvar *profiled-functions* nil
  "List of currently profiled functions.")

(defun introspect-profile-functions (action &key functions package)
  "Manage deterministic profiling of functions.
ACTION can be:
  :start - Start profiling FUNCTIONS (list of names or PACKAGE string)
  :stop - Stop profiling all functions
  :report - Get profiling report
  :reset - Reset profiling statistics
  :status - Show currently profiled functions"
  (case action
    (:start
     (when *profiled-functions*
       (sb-profile:unprofile))
     (setf *profiled-functions* nil)
     (cond
       (functions
        ;; Profile specific functions
        (dolist (fn-name functions)
          (let ((sym (if (symbolp fn-name)
                         fn-name
                         (let ((pkg (or (find-package "CL-USER") *package*)))
                           (find-symbol (string-upcase fn-name) pkg)))))
            (when (and sym (fboundp sym))
              (eval `(sb-profile:profile ,sym))
              (push sym *profiled-functions*)))))
       (package
        ;; Profile all functions in package
        (eval `(sb-profile:profile ,package))
        (setf *profiled-functions* (list package))))
     (list :action :start
           :profiled *profiled-functions*
           :count (length *profiled-functions*)))

    (:stop
     (let ((was-profiled *profiled-functions*))
       (sb-profile:unprofile)
       (setf *profiled-functions* nil)
       (list :action :stop
             :unprofiled was-profiled)))

    (:report
     (list :action :report
           :profiled *profiled-functions*
           :report (with-output-to-string (*standard-output*)
                     (sb-profile:report))))

    (:reset
     (sb-profile:reset)
     (list :action :reset
           :profiled *profiled-functions*))

    (:status
     (list :action :status
           :profiled *profiled-functions*
           :active (not (null *profiled-functions*))))))

(defun format-profile-functions-result (info)
  "Format deterministic profiling result as human-readable string."
  (with-output-to-string (s)
    (case (getf info :action)
      (:start
       (format s "Started profiling ~D function~:P:~%"
               (getf info :count))
       (dolist (fn (getf info :profiled))
         (format s "  ~A~%" fn)))
      (:stop
       (format s "Stopped profiling.~%Previously profiled:~%")
       (dolist (fn (getf info :unprofiled))
         (format s "  ~A~%" fn)))
      (:report
       (if (getf info :profiled)
           (format s "~A" (getf info :report))
           (format s "No functions are being profiled.")))
      (:reset
       (format s "Profiling statistics reset."))
      (:status
       (if (getf info :active)
           (progn
             (format s "Currently profiling:~%")
             (dolist (fn (getf info :profiled))
               (format s "  ~A~%" fn)))
           (format s "No functions are being profiled."))))))

;;; ==========================================================================
;;; Memory Analysis
;;; ==========================================================================

(defun introspect-memory-report (&key (verbosity :default) gc-first)
  "Get memory usage report.
VERBOSITY can be :DEFAULT, T (detailed), or NIL (minimal).
If GC-FIRST is true, run garbage collection before reporting."
  (when gc-first
    (sb-ext:gc :full t))
  (let ((room-output (with-output-to-string (*standard-output*)
                       (room verbosity)))
        (gc-run-time sb-ext:*gc-run-time*)
        (gc-real-time (if (boundp 'sb-ext:*gc-real-time*)
                          (symbol-value 'sb-ext:*gc-real-time*)
                          nil))
        (bytes-consed (ignore-errors (sb-ext:get-bytes-consed))))
    (list :room room-output
          :gc-run-time-ms (/ gc-run-time 1000.0)
          :gc-real-time-ms (when gc-real-time (/ gc-real-time 1000.0))
          :bytes-consed bytes-consed
          :gc-triggered gc-first)))

(defun format-memory-report (info)
  "Format memory report as human-readable string."
  (with-output-to-string (s)
    (format s "~A~%" (getf info :room))
    (format s "GC Statistics:~%")
    (format s "  Run time: ~,2F ms~%" (getf info :gc-run-time-ms))
    (when (getf info :gc-real-time-ms)
      (format s "  Real time: ~,2F ms~%" (getf info :gc-real-time-ms)))
    (when (getf info :bytes-consed)
      (format s "  Total bytes consed: ~:D~%" (getf info :bytes-consed)))
    (when (getf info :gc-triggered)
      (format s "  (GC was run before this report)~%"))))

;;; ==========================================================================
;;; Allocation Profiling
;;; ==========================================================================

(defun introspect-allocation-profile (code &key (max-samples 1000) (package "CL-USER"))
  "Profile memory allocation in CODE using sb-sprof :alloc mode.
Returns detailed allocation information."
  (let* ((pkg (or (find-package (string-upcase package))
                  (error "Package ~A not found" package)))
         (*package* pkg)
         (report-string nil)
         (result-values nil)
         (error-message nil)
         (bytes-before (ignore-errors (sb-ext:get-bytes-consed))))
    ;; Parse the code
    (let ((forms (with-input-from-string (s code)
                   (loop for form = (read s nil 'eof)
                         until (eq form 'eof)
                         collect form))))
      ;; Run with allocation profiling
      (sb-sprof:reset)
      (handler-case
          (sb-sprof:with-profiling (:max-samples max-samples
                                    :mode :alloc
                                    :reset t
                                    :report nil)
            (dolist (form forms)
              (setf result-values (multiple-value-list (eval form)))))
        (error (e)
          (setf error-message (format nil "~A" e))))
      ;; Capture report
      (setf report-string
            (with-output-to-string (*standard-output*)
              (sb-sprof:report :type :flat :max 30))))
    (let ((bytes-after (ignore-errors (sb-ext:get-bytes-consed))))
      (list :success (null error-message)
            :result (if error-message
                        error-message
                        (format nil "~{~S~^~%~}" result-values))
            :bytes-allocated (when (and bytes-before bytes-after)
                               (- bytes-after bytes-before))
            :max-samples max-samples
            :report report-string))))

(defun format-allocation-profile-result (info)
  "Format allocation profiling result as human-readable string."
  (with-output-to-string (s)
    (if (getf info :success)
        (progn
          (format s "Allocation profiling completed~%~%")
          (format s "Result: ~A~%~%" (getf info :result))
          (when (getf info :bytes-allocated)
            (format s "Approximate bytes allocated: ~:D~%~%"
                    (getf info :bytes-allocated)))
          (format s "Allocation Report:~%~A" (getf info :report)))
        (format s "Allocation profiling failed: ~A~%~%~A"
                (getf info :result)
                (getf info :report)))))
