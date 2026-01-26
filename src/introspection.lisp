;;; src/introspection.lisp
;;; ABOUTME: SBCL-specific introspection tools for symbol/function analysis
;;;
;;; Provides deep inspection of the running Lisp image:
;;; - Symbol information (type, documentation, arglist, source location)
;;; - Symbol search (apropos with type filtering)
;;; - Cross-reference queries (who-calls, who-references)
;;; - Macro expansion

(in-package #:cl-mcp-server.introspection)

;;; ==========================================================================
;;; Symbol Type Classification
;;; ==========================================================================

(defun symbol-type-info (sym)
  "Return a keyword describing what SYM names.
Returns one of: :macro, :generic-function, :function, :class, :variable, :symbol."
  (cond
    ((and (fboundp sym) (macro-function sym)) :macro)
    ((and (fboundp sym) (typep (fdefinition sym) 'generic-function)) :generic-function)
    ((fboundp sym) :function)
    ((find-class sym nil) :class)
    ((boundp sym) :variable)
    (t :symbol)))

;;; ==========================================================================
;;; A.1: describe-symbol - Comprehensive Symbol Information
;;; ==========================================================================

(defun introspect-symbol (sym)
  "Get comprehensive information about symbol SYM.
Returns a plist with:
  :name - symbol name string
  :package - package name string (or nil if uninterned)
  :type - keyword classifying what the symbol names
  :documentation - docstring if available
  :arglist - lambda list for functions/macros
  :value - printed representation for bound variables
  :source-file - definition source file path
  :source-offset - character offset in source file"
  (let ((info (list :name (symbol-name sym)
                    :package (when (symbol-package sym)
                               (package-name (symbol-package sym)))
                    :type (symbol-type-info sym))))
    ;; Add documentation
    (let ((doc (or (documentation sym 'function)
                   (documentation sym 'variable)
                   (documentation sym 'type))))
      (when doc
        (setf (getf info :documentation) doc)))
    ;; Add arglist for functions/macros
    (when (fboundp sym)
      (let ((arglist (ignore-errors
                       (sb-introspect:function-lambda-list sym))))
        (when arglist
          (setf (getf info :arglist) arglist))))
    ;; Add value for variables (with print safety)
    (when (boundp sym)
      (setf (getf info :value)
            (let ((*print-length* 20)
                  (*print-level* 3)
                  (*print-circle* t))
              (handler-case
                  (prin1-to-string (symbol-value sym))
                (error () "<error printing value>")))))
    ;; Add source location using sb-introspect
    (let ((sources (ignore-errors
                     (sb-introspect:find-definition-sources-by-name
                      sym
                      (case (symbol-type-info sym)
                        ((:function :generic-function :macro) :function)
                        (:class :class)
                        (:variable :variable)
                        (t :function))))))
      (when (and sources (first sources))
        (let ((source (first sources)))
          (let ((pathname (sb-introspect:definition-source-pathname source)))
            (when pathname
              (setf (getf info :source-file) (namestring pathname))))
          (let ((offset (sb-introspect:definition-source-character-offset source)))
            (when offset
              (setf (getf info :source-offset) offset))))))
    info))

(defun format-symbol-info (info)
  "Format symbol info plist as human-readable string."
  (with-output-to-string (s)
    (format s "~A::~A [~A]~%"
            (or (getf info :package) "#")
            (getf info :name)
            (getf info :type))
    (when (getf info :arglist)
      (format s "  Arglist: ~S~%" (getf info :arglist)))
    (when (getf info :value)
      (format s "  Value: ~A~%" (getf info :value)))
    (when (getf info :documentation)
      (format s "  Documentation:~%    ~A~%"
              (getf info :documentation)))
    (when (getf info :source-file)
      (format s "  Source: ~A~@[:~A~]~%"
              (getf info :source-file)
              (getf info :source-offset)))))

;;; ==========================================================================
;;; A.2: apropos-search - Symbol Discovery
;;; ==========================================================================

(defun introspect-apropos (pattern &key package type)
  "Search for symbols matching PATTERN (case-insensitive substring).
PACKAGE limits search to a specific package (string or package).
TYPE filters by :function, :variable, :macro, :class, or :generic-function.
Returns list of plists with :name, :package, :type."
  (let ((results '())
        (pkg (when package
               (etypecase package
                 (package package)
                 (string (find-package (string-upcase package)))
                 (symbol (find-package package))))))
    (when (and package (not pkg))
      (return-from introspect-apropos nil))
    (flet ((matches-type-p (sym)
             (or (null type)
                 (eq type (symbol-type-info sym))))
           (add-result (sym)
             (push (list :name (symbol-name sym)
                         :package (when (symbol-package sym)
                                    (package-name (symbol-package sym)))
                         :type (symbol-type-info sym))
                   results)))
      (if pkg
          ;; Search specific package (all symbols)
          (do-symbols (sym pkg)
            (when (and (search pattern (symbol-name sym) :test #'char-equal)
                       (matches-type-p sym))
              (add-result sym)))
          ;; Search all packages (external symbols only)
          (dolist (p (list-all-packages))
            (do-external-symbols (sym p)
              (when (and (search pattern (symbol-name sym) :test #'char-equal)
                         (matches-type-p sym))
                (add-result sym))))))
    ;; Sort by name and remove duplicates
    (remove-duplicates
     (sort results #'string< :key (lambda (r) (getf r :name)))
     :test #'equal
     :key (lambda (r) (cons (getf r :name) (getf r :package))))))

(defun format-apropos-results (results pattern)
  "Format apropos results as human-readable string."
  (with-output-to-string (s)
    (format s "Found ~D symbol~:P matching '~A':~%~%"
            (length results) pattern)
    (dolist (r results)
      (format s "  ~A::~A [~A]~%"
              (or (getf r :package) "#")
              (getf r :name)
              (getf r :type)))))

;;; ==========================================================================
;;; A.3: who-calls - Cross-Reference (Callers)
;;; ==========================================================================

(defun introspect-who-calls (sym)
  "Find all functions that call SYM.
Uses SBCL's cross-reference database.
Returns list of plists with :caller, :caller-package, :location."
  (let ((callers (sb-introspect:who-calls sym)))
    (mapcar (lambda (entry)
              (let* ((caller (car entry))
                     (source (cdr entry))
                     (pathname (when source
                                 (sb-introspect:definition-source-pathname source))))
                (list :caller (if (symbolp caller)
                                  (symbol-name caller)
                                  (prin1-to-string caller))
                      :caller-package (when (symbolp caller)
                                        (and (symbol-package caller)
                                             (package-name (symbol-package caller))))
                      :location (when pathname (namestring pathname)))))
            callers)))

(defun format-who-calls-results (results sym)
  "Format who-calls results as human-readable string."
  (with-output-to-string (s)
    (if results
        (progn
          (format s "~D caller~:P of ~A:~%~%" (length results) sym)
          (dolist (r results)
            (format s "  ~@[~A::~]~A~%"
                    (getf r :caller-package)
                    (getf r :caller))
            (when (getf r :location)
              (format s "    at ~A~%" (getf r :location)))))
        (format s "No callers found for ~A~%" sym))))

;;; ==========================================================================
;;; who-references - Cross-Reference (Variable References)
;;; ==========================================================================

(defun introspect-who-references (sym)
  "Find all code that references (reads) SYM.
Uses SBCL's cross-reference database.
Returns list of plists with :referencer, :referencer-package, :location."
  (let ((refs (sb-introspect:who-references sym)))
    (mapcar (lambda (entry)
              (let* ((referencer (car entry))
                     (source (cdr entry))
                     (pathname (when source
                                 (sb-introspect:definition-source-pathname source))))
                (list :referencer (if (symbolp referencer)
                                      (symbol-name referencer)
                                      (prin1-to-string referencer))
                      :referencer-package (when (symbolp referencer)
                                            (and (symbol-package referencer)
                                                 (package-name (symbol-package referencer))))
                      :location (when pathname (namestring pathname)))))
            refs)))

(defun format-who-references-results (results sym)
  "Format who-references results as human-readable string."
  (with-output-to-string (s)
    (if results
        (progn
          (format s "~D reference~:P to ~A:~%~%" (length results) sym)
          (dolist (r results)
            (format s "  ~@[~A::~]~A~%"
                    (getf r :referencer-package)
                    (getf r :referencer))
            (when (getf r :location)
              (format s "    at ~A~%" (getf r :location)))))
        (format s "No references found for ~A~%" sym))))

;;; ==========================================================================
;;; A.4: macroexpand-form - Macro Expansion
;;; ==========================================================================

(defun introspect-macroexpand (form-string &key full (package *package*))
  "Expand macros in FORM-STRING.
If FULL is true, use macroexpand (recursive), otherwise macroexpand-1.
PACKAGE sets the read context.
Returns plist with :original, :expanded, :changed-p."
  (let* ((*package* (etypecase package
                      (package package)
                      (string (or (find-package (string-upcase package))
                                  (error "Package ~A not found" package)))
                      (symbol (or (find-package package)
                                  (error "Package ~A not found" package)))))
         (form (read-from-string form-string))
         (expanded (if full
                       (macroexpand form)
                       (macroexpand-1 form))))
    (list :original (let ((*print-pretty* t))
                      (prin1-to-string form))
          :expanded (let ((*print-pretty* t)
                          (*print-right-margin* 80))
                      (prin1-to-string expanded))
          :changed-p (not (eq form expanded)))))

(defun format-macroexpand-result (result)
  "Format macroexpand result as human-readable string."
  (with-output-to-string (s)
    (format s "Original:~%~A~%~%"
            (getf result :original))
    (if (getf result :changed-p)
        (format s "Expanded:~%~A~%"
                (getf result :expanded))
        (format s "(No expansion - not a macro form)~%"))))

;;; ==========================================================================
;;; Helper: Resolve Symbol from String
;;; ==========================================================================

(defun resolve-symbol (name &optional package-name)
  "Resolve a symbol from NAME string in PACKAGE-NAME.
Returns the symbol or signals an error if not found."
  (let* ((pkg (if package-name
                  (or (find-package (string-upcase package-name))
                      (error "Package ~A not found" package-name))
                  *package*))
         (sym (find-symbol (string-upcase name) pkg)))
    (unless sym
      (error "Symbol ~A not found in package ~A" name (package-name pkg)))
    sym))
