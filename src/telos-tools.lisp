;;; src/telos-tools.lisp
;;; ABOUTME: Telos intent introspection tools for MCP
;;;
;;; Provides tools for querying code intent when telos is loaded.
;;; All functions gracefully handle the case where telos is not available.

(in-package #:cl-mcp-server.telos-tools)

;;; ==========================================================================
;;; Telos Availability Check
;;; ==========================================================================

(defun telos-available-p ()
  "Return T if telos is loaded and available."
  (find-package :telos))

;;; ==========================================================================
;;; Introspection Functions
;;; ==========================================================================

(defun introspect-list-features (&optional filter)
  "List all defined features, optionally filtered by name substring.
Returns a list of feature info plists."
  (unless (telos-available-p)
    (return-from introspect-list-features nil))
  (let* ((list-fn (uiop:find-symbol* :list-features :telos))
         (feature-intent-fn (uiop:find-symbol* :feature-intent :telos))
         (features (funcall list-fn filter)))
    (loop for feat in features
          for intent = (funcall feature-intent-fn feat)
          collect (list :name feat
                        :purpose (when intent
                                   (slot-value intent
                                               (uiop:find-symbol* :purpose :telos)))
                        :belongs-to (when intent
                                      (slot-value intent
                                                  (uiop:find-symbol* :belongs-to :telos)))))))

(defun introspect-feature-intent (feature-name)
  "Get full intent for a feature by name.
FEATURE-NAME can be a string or symbol."
  (unless (telos-available-p)
    (return-from introspect-feature-intent nil))
  (let* ((feature-intent-fn (uiop:find-symbol* :feature-intent :telos))
         (feat-sym (if (stringp feature-name)
                       (or (find-symbol (string-upcase feature-name))
                           (intern (string-upcase feature-name) :keyword))
                       feature-name))
         (intent (funcall feature-intent-fn feat-sym)))
    (when intent
      (intent-to-plist intent))))

(defun introspect-get-intent (symbol-name &optional package-name)
  "Get intent for a function, class, or condition.
Returns nil if no intent is attached."
  (unless (telos-available-p)
    (return-from introspect-get-intent nil))
  (let* ((get-intent-fn (uiop:find-symbol* :get-intent :telos))
         (class-intent-fn (uiop:find-symbol* :class-intent :telos))
         (pkg (if package-name
                  (find-package (string-upcase package-name))
                  *package*))
         (sym (find-symbol (string-upcase symbol-name) pkg)))
    (unless sym
      (return-from introspect-get-intent
        (list :error (format nil "Symbol ~A not found in ~A"
                             symbol-name (or package-name "current package")))))
    ;; Try function intent first
    (let ((intent (ignore-errors (funcall get-intent-fn sym))))
      (when intent
        (return-from introspect-get-intent
          (list* :type :function :name sym (intent-to-plist intent)))))
    ;; Try class intent
    (let ((class (ignore-errors (find-class sym nil))))
      (when class
        (let ((intent (ignore-errors (funcall class-intent-fn class))))
          (when intent
            (return-from introspect-get-intent
              (list* :type :class :name sym (intent-to-plist intent)))))))
    ;; No intent found
    (list :type :unknown :name sym :intent nil)))

(defun introspect-intent-chain (symbol-name &optional package-name)
  "Trace intent from symbol up to root feature.
Returns list of intent entries from most specific to root."
  (unless (telos-available-p)
    (return-from introspect-intent-chain nil))
  (let* ((intent-chain-fn (uiop:find-symbol* :intent-chain :telos))
         (pkg (if package-name
                  (find-package (string-upcase package-name))
                  *package*))
         (sym (find-symbol (string-upcase symbol-name) pkg)))
    (unless sym
      (return-from introspect-intent-chain
        (list :error (format nil "Symbol ~A not found" symbol-name))))
    (let ((chain (ignore-errors (funcall intent-chain-fn sym))))
      (or chain
          (list :info "No intent chain found for this symbol")))))

(defun introspect-feature-members (feature-name)
  "List all functions and classes belonging to a feature."
  (unless (telos-available-p)
    (return-from introspect-feature-members nil))
  (let* ((feature-members-fn (uiop:find-symbol* :feature-members :telos))
         (feat-sym (if (stringp feature-name)
                       (or (find-symbol (string-upcase feature-name))
                           (intern (string-upcase feature-name) :keyword))
                       feature-name)))
    (ignore-errors (funcall feature-members-fn feat-sym))))

(defun introspect-feature-decisions (feature-name)
  "Get decisions for a feature by name.
FEATURE-NAME can be a string or symbol."
  (unless (telos-available-p)
    (return-from introspect-feature-decisions nil))
  (let* ((feature-decisions-fn (uiop:find-symbol* :feature-decisions :telos))
         (feat-sym (if (stringp feature-name)
                       (or (find-symbol (string-upcase feature-name))
                           (intern (string-upcase feature-name) :keyword))
                       feature-name))
         (decisions (ignore-errors (funcall feature-decisions-fn feat-sym))))
    (when decisions
      (mapcar #'decision-to-plist decisions))))

(defun introspect-list-decisions ()
  "Get all decisions across all features.
Returns alist of (feature-name-string . list-of-decision-plists)."
  (unless (telos-available-p)
    (return-from introspect-list-decisions nil))
  (let* ((list-decisions-fn (uiop:find-symbol* :list-decisions :telos))
         (all-decisions (ignore-errors (funcall list-decisions-fn))))
    (when all-decisions
      (loop for (feature . decisions) in all-decisions
            collect (cons (symbol-name feature)
                          (mapcar #'decision-to-plist decisions))))))

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

(defun intent-to-plist (intent)
  "Convert a telos:intent struct to a plist for formatting."
  (let ((purpose-slot (uiop:find-symbol* :purpose :telos))
        (role-slot (uiop:find-symbol* :role :telos))
        (belongs-to-slot (uiop:find-symbol* :belongs-to :telos))
        (goals-slot (uiop:find-symbol* :goals :telos))
        (constraints-slot (uiop:find-symbol* :constraints :telos))
        (assumptions-slot (uiop:find-symbol* :assumptions :telos))
        (failure-modes-slot (uiop:find-symbol* :failure-modes :telos)))
    (list :purpose (slot-value intent purpose-slot)
          :role (slot-value intent role-slot)
          :belongs-to (slot-value intent belongs-to-slot)
          :goals (slot-value intent goals-slot)
          :constraints (slot-value intent constraints-slot)
          :assumptions (slot-value intent assumptions-slot)
          :failure-modes (slot-value intent failure-modes-slot))))

(defun decision-to-plist (decision)
  "Convert a telos:decision struct to a plist for formatting."
  (let ((id-acc (uiop:find-symbol* :decision-id :telos))
        (chose-acc (uiop:find-symbol* :decision-chose :telos))
        (over-acc (uiop:find-symbol* :decision-over :telos))
        (because-acc (uiop:find-symbol* :decision-because :telos))
        (date-acc (uiop:find-symbol* :decision-date :telos))
        (decided-by-acc (uiop:find-symbol* :decision-decided-by :telos)))
    (list :id (funcall id-acc decision)
          :chose (funcall chose-acc decision)
          :over (funcall over-acc decision)
          :because (funcall because-acc decision)
          :date (funcall date-acc decision)
          :decided-by (funcall decided-by-acc decision))))

;;; ==========================================================================
;;; Formatting Functions
;;; ==========================================================================

(defun format-list-features (features)
  "Format feature list for output."
  (if (null features)
      "No features defined (or telos not loaded)."
      (with-output-to-string (s)
        (format s "Features (~D):~%~%" (length features))
        (dolist (f features)
          (format s "~A~%" (getf f :name))
          (when (getf f :purpose)
            (format s "  Purpose: ~A~%" (getf f :purpose)))
          (when (getf f :belongs-to)
            (format s "  Parent: ~A~%" (getf f :belongs-to)))
          (format s "~%")))))

(defun format-feature-intent (intent-plist feature-name)
  "Format a feature's intent for output."
  (if (null intent-plist)
      (format nil "Feature ~A not found (or telos not loaded)." feature-name)
      (with-output-to-string (s)
        (format s "Feature: ~A~%~%" feature-name)
        (format-intent-fields s intent-plist))))

(defun format-get-intent (result)
  "Format intent query result."
  (cond
    ((null result)
     "Telos not loaded.")
    ((getf result :error)
     (format nil "Error: ~A" (getf result :error)))
    ((null (getf result :purpose))
     (format nil "No intent defined for ~A." (getf result :name)))
    (t
     (with-output-to-string (s)
       (format s "~A (~A)~%~%" (getf result :name) (getf result :type))
       (format-intent-fields s result)))))

(defun format-intent-chain (chain)
  "Format intent chain for output."
  (cond
    ((null chain)
     "Telos not loaded.")
    ((getf (first chain) :error)
     (format nil "Error: ~A" (getf (first chain) :error)))
    ((getf (first chain) :info)
     (getf (first chain) :info))
    (t
     (with-output-to-string (s)
       (format s "Intent Chain (~D levels):~%~%" (length chain))
       (loop for entry in chain
             for i from 1
             do (format s "~D. [~A] ~A~%"
                        i
                        (getf entry :type)
                        (getf entry :name))
                (when (getf entry :role)
                  (format s "   Role: ~A~%" (getf entry :role)))
                (when (getf entry :purpose)
                  (format s "   Purpose: ~A~%" (getf entry :purpose)))
                (when (getf entry :failure-modes)
                  (format s "   Failure modes: ~{~A~^, ~}~%"
                          (mapcar #'first (getf entry :failure-modes))))
                (format s "~%"))))))

(defun format-feature-members (members feature-name)
  "Format feature members for output."
  (if (null members)
      (format nil "Feature ~A not found (or telos not loaded)." feature-name)
      (with-output-to-string (s)
        (format s "Members of ~A:~%~%" feature-name)
        (let ((functions (getf members :functions))
              (classes (getf members :classes))
              (structs (getf members :structs))
              (conditions (getf members :conditions))
              (methods (getf members :methods))
              (features (getf members :features)))
          (when functions
            (format s "Functions (~D):~%" (length functions))
            (dolist (fn functions)
              (format s "  ~A~%" fn))
            (format s "~%"))
          (when classes
            (format s "Classes (~D):~%" (length classes))
            (dolist (cls classes)
              (format s "  ~A~%" cls))
            (format s "~%"))
          (when structs
            (format s "Structs (~D):~%" (length structs))
            (dolist (st structs)
              (format s "  ~A~%" st))
            (format s "~%"))
          (when conditions
            (format s "Conditions (~D):~%" (length conditions))
            (dolist (cond conditions)
              (format s "  ~A~%" cond))
            (format s "~%"))
          (when methods
            (format s "Methods (~D):~%" (length methods))
            (dolist (m methods)
              (format s "  ~A~%" m))
            (format s "~%"))
          (when features
            (format s "Sub-features (~D):~%" (length features))
            (dolist (feat features)
              (format s "  ~A~%" feat)))))))

(defun format-feature-decisions (decisions feature-name)
  "Format a feature's decisions for output."
  (if (null decisions)
      (format nil "No decisions found for ~A (or telos not loaded)." feature-name)
      (with-output-to-string (s)
        (format s "Decisions for ~A (~D):~%~%" feature-name (length decisions))
        (loop for dec in decisions
              for i from 1
              do (format s "~D. ~A~%" i (or (getf dec :id) "(unnamed)"))
                 (when (getf dec :chose)
                   (format s "   Chose: ~A~%" (getf dec :chose)))
                 (when (getf dec :over)
                   (format s "   Over: ~{~A~^, ~}~%" (getf dec :over)))
                 (when (getf dec :because)
                   (format s "   Because: ~A~%" (getf dec :because)))
                 (when (getf dec :decided-by)
                   (format s "   Decided by: ~A~%" (getf dec :decided-by)))
                 (when (getf dec :date)
                   (format s "   Date: ~A~%" (getf dec :date)))
                 (format s "~%")))))

(defun format-list-decisions (all-decisions)
  "Format all decisions across features for output."
  (if (null all-decisions)
      "No decisions recorded (or telos not loaded)."
      (with-output-to-string (s)
        (let ((total (loop for (_ . decs) in all-decisions sum (length decs))))
          (format s "Decisions across ~D feature~:P (~D total):~%~%"
                  (length all-decisions) total))
        (loop for (feature-name . decisions) in all-decisions
              do (format s "~A (~D):~%" feature-name (length decisions))
                 (dolist (dec decisions)
                   (format s "  ~A: chose ~A"
                           (or (getf dec :id) "(unnamed)")
                           (or (getf dec :chose) "?"))
                   (when (getf dec :over)
                     (format s " over ~{~A~^, ~}" (getf dec :over)))
                   (format s "~%"))
                 (format s "~%")))))

(defun format-intent-fields (stream plist)
  "Format intent fields to a stream."
  (when (getf plist :purpose)
    (format stream "Purpose: ~A~%~%" (getf plist :purpose)))
  (when (getf plist :role)
    (format stream "Role: ~A~%~%" (getf plist :role)))
  (when (getf plist :belongs-to)
    (format stream "Belongs to: ~A~%~%" (getf plist :belongs-to)))
  (when (getf plist :goals)
    (format stream "Goals:~%")
    (dolist (g (getf plist :goals))
      (format stream "  ~A: ~A~%" (first g) (second g)))
    (format stream "~%"))
  (when (getf plist :constraints)
    (format stream "Constraints:~%")
    (dolist (c (getf plist :constraints))
      (format stream "  ~A: ~A~%" (first c) (second c)))
    (format stream "~%"))
  (when (getf plist :assumptions)
    (format stream "Assumptions:~%")
    (dolist (a (getf plist :assumptions))
      (format stream "  ~A: ~A~%" (first a) (second a)))
    (format stream "~%"))
  (when (getf plist :failure-modes)
    (format stream "Failure Modes:~%")
    (dolist (f (getf plist :failure-modes))
      (format stream "  ~A: ~A~%" (first f) (second f)))))
