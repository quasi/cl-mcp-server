;;; src/introspection.lisp
;;; ABOUTME: SBCL-specific introspection tools for symbol/function analysis
;;;
;;; Provides deep inspection of the running Lisp image:
;;; - Symbol information (type, documentation, arglist, source location)
;;; - Symbol search (apropos with type filtering)
;;; - Cross-reference queries (who-calls, who-references)
;;; - Macro expansion

;; Ensure sb-introspect is available (SBCL-specific)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

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
;;; B.2: compile-form - Compilation without Evaluation
;;; ==========================================================================

(defun introspect-compile-form (form-string &key (package *package*))
  "Compile code in FORM-STRING without executing it.
Catches compilation warnings and errors that wouldn't appear during simple evaluation.
PACKAGE sets the read context.
Returns plist with :compiled-p :warnings :errors :notes."
  (let* ((pkg (etypecase package
                (package package)
                (string (or (find-package (string-upcase package))
                            (error "Package ~A not found" package)))
                (symbol (or (find-package package)
                            (error "Package ~A not found" package)))))
         (*package* pkg)
         (warnings nil)
         (errors nil)
         (notes nil)
         (form (handler-case
                   (read-from-string form-string)
                 (error (e)
                   (return-from introspect-compile-form
                     (list :compiled-p nil
                           :errors (list (format nil "Read error: ~A" e))
                           :warnings nil
                           :notes nil))))))
    ;; Wrap the form in a lambda so COMPILE can handle it
    (let ((lambda-form `(lambda () ,form)))
      (handler-bind
          ((sb-ext:compiler-note
             (lambda (c)
               (push (format nil "~A" c) notes)
               (muffle-warning c)))
           (style-warning
             (lambda (c)
               (push (format nil "[STYLE] ~A" c) warnings)
               (muffle-warning c)))
           (warning
             (lambda (c)
               (push (format nil "~A" c) warnings)
               (muffle-warning c))))
        (handler-case
            (multiple-value-bind (fn had-warnings had-errors)
                (compile nil lambda-form)
              (declare (ignore fn))
              (list :compiled-p t
                    :had-warnings had-warnings
                    :had-errors had-errors
                    :warnings (nreverse warnings)
                    :errors (nreverse errors)
                    :notes (nreverse notes)))
          (error (e)
            (list :compiled-p nil
                  :errors (list (format nil "~A" e))
                  :warnings (nreverse warnings)
                  :notes (nreverse notes))))))))

(defun format-compile-result (result)
  "Format compile result as human-readable string."
  (with-output-to-string (s)
    (if (getf result :compiled-p)
        (format s "Compilation successful.~%")
        (format s "Compilation FAILED.~%"))
    (let ((errors (getf result :errors)))
      (when errors
        (format s "~%Errors (~D):~%" (length errors))
        (dolist (e errors)
          (format s "  ~A~%" e))))
    (let ((warnings (getf result :warnings)))
      (when warnings
        (format s "~%Warnings (~D):~%" (length warnings))
        (dolist (w warnings)
          (format s "  ~A~%" w))))
    (let ((notes (getf result :notes)))
      (when notes
        (format s "~%Compiler notes (~D):~%" (length notes))
        (dolist (n notes)
          (format s "  ~A~%" n))))
    (when (and (getf result :compiled-p)
               (not (getf result :warnings))
               (not (getf result :notes)))
      (format s "No warnings or notes.~%"))))

;;; ==========================================================================
;;; A.5: validate-syntax - Syntax Validation Without Evaluation
;;; ==========================================================================

(defun introspect-validate-syntax (code-string)
  "Validate that CODE-STRING is syntactically valid Lisp without evaluating it.
Returns a plist with:
  :valid - T if syntax is correct
  :forms - Number of top-level forms if valid
  :error - Error description if invalid
  :unclosed-count - Number of unclosed parens (if applicable)
  :line-hint - Approximate line of problem (if applicable)"
  (handler-case
      (with-input-from-string (stream code-string)
        (let ((count 0))
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (incf count))
          (list :valid t :forms count)))
    (end-of-file (c)
      (declare (ignore c))
      ;; Count unclosed parens to estimate problem
      (let ((depth 0)
            (line 1)
            (in-string nil))
        (loop for i from 0 below (length code-string)
              for char = (char code-string i)
              for prev = (if (> i 0) (char code-string (1- i)) #\Space)
              do (case char
                   (#\Newline (incf line))
                   (#\" (unless (char= prev #\\) (setf in-string (not in-string))))
                   (#\( (unless in-string (incf depth)))
                   (#\) (unless in-string (decf depth)))))
        (list :valid nil
              :error "Unexpected end of input - unclosed parenthesis"
              :unclosed-count depth
              :line-hint line)))
    (sb-int:simple-reader-error (c)
      ;; SBCL-specific reader error with better info
      (list :valid nil
            :error (format nil "~A" c)))
    (reader-error (c)
      (list :valid nil
            :error (format nil "Reader error: ~A" c)))
    (error (c)
      (list :valid nil
            :error (format nil "~A" c)))))

(defun format-validate-result (result)
  "Format validation result as human-readable string."
  (with-output-to-string (s)
    (if (getf result :valid)
        (format s "✓ Syntax valid: ~D top-level form~:P~%"
                (getf result :forms))
        (progn
          (format s "✗ Syntax invalid~%~%")
          (format s "Error: ~A~%" (getf result :error))
          (when (getf result :unclosed-count)
            (format s "Unclosed parentheses: ~D~%" (getf result :unclosed-count)))
          (when (getf result :line-hint)
            (format s "Approximate location: line ~D~%" (getf result :line-hint)))))))

;;; ==========================================================================
;;; D.1: class-info - CLOS Class Introspection
;;; ==========================================================================

(defun introspect-slot (slot-def)
  "Extract information from a slot definition.
Works for both direct and effective slot definitions.
Returns a plist with :name, :type, :initargs, :initform, :readers, :writers, :allocation."
  (let ((is-direct (typep slot-def 'sb-mop:direct-slot-definition)))
    (list :name (sb-mop:slot-definition-name slot-def)
          :type (let ((type (sb-mop:slot-definition-type slot-def)))
                  (if (eq type t) nil type))
          :initargs (sb-mop:slot-definition-initargs slot-def)
          :initform (when is-direct
                      (let ((fn (sb-mop:slot-definition-initfunction slot-def)))
                        (if fn
                            (let ((*print-length* 10)
                                  (*print-level* 3))
                              (handler-case
                                  (prin1-to-string (funcall fn))
                                (error () "<error evaluating initform>")))
                            nil)))
          :readers (when is-direct
                     (sb-mop:slot-definition-readers slot-def))
          :writers (when is-direct
                     (sb-mop:slot-definition-writers slot-def))
          :allocation (sb-mop:slot-definition-allocation slot-def))))

(defun introspect-class (class-designator)
  "Get comprehensive information about a CLOS class.
CLASS-DESIGNATOR can be a class object, symbol naming a class, or string.
Returns a plist with:
  :name - class name symbol
  :package - package name string
  :metaclass - metaclass name
  :documentation - class documentation
  :superclasses - list of direct superclass names
  :subclasses - list of direct subclass names
  :precedence-list - class precedence list (all superclasses)
  :direct-slots - slots defined directly on this class
  :effective-slots - all slots including inherited
  :default-initargs - default initialization arguments"
  (let* ((class (etypecase class-designator
                  (class class-designator)
                  (symbol (or (find-class class-designator nil)
                              (error "Class ~A not found" class-designator)))
                  (string (let ((sym (find-symbol (string-upcase class-designator))))
                            (if sym
                                (or (find-class sym nil)
                                    (error "~A is not a class" class-designator))
                                (error "Symbol ~A not found" class-designator))))))
         (name (class-name class)))
    ;; Ensure class is finalized so we can get effective slots
    (unless (sb-mop:class-finalized-p class)
      (sb-mop:finalize-inheritance class))
    (list :name name
          :package (when (and name (symbol-package name))
                     (package-name (symbol-package name)))
          :metaclass (class-name (class-of class))
          :documentation (documentation class t)
          :superclasses (mapcar #'class-name
                                (sb-mop:class-direct-superclasses class))
          :subclasses (mapcar #'class-name
                              (sb-mop:class-direct-subclasses class))
          :precedence-list (mapcar #'class-name
                                   (sb-mop:class-precedence-list class))
          :direct-slots (mapcar #'introspect-slot
                                (sb-mop:class-direct-slots class))
          :effective-slots (mapcar #'introspect-slot
                                   (sb-mop:class-slots class))
          :default-initargs (let ((initargs (sb-mop:class-default-initargs class)))
                              (mapcar (lambda (ia)
                                        (list :key (first ia)
                                              :value (let ((*print-length* 10)
                                                           (*print-level* 3))
                                                       (handler-case
                                                           (prin1-to-string
                                                            (funcall (third ia)))
                                                         (error () "<error>")))))
                                      initargs)))))

(defun format-slot-info (slot &key (indent "  "))
  "Format a slot info plist as a human-readable string."
  (with-output-to-string (s)
    (format s "~A~A" indent (getf slot :name))
    (when (getf slot :type)
      (format s " : ~S" (getf slot :type)))
    (format s "~%")
    (when (getf slot :initargs)
      (format s "~A  initargs: ~{~S~^, ~}~%" indent (getf slot :initargs)))
    (when (getf slot :initform)
      (format s "~A  initform: ~A~%" indent (getf slot :initform)))
    (when (getf slot :readers)
      (format s "~A  readers: ~{~A~^, ~}~%" indent (getf slot :readers)))
    (when (getf slot :writers)
      (format s "~A  writers: ~{~A~^, ~}~%" indent (getf slot :writers)))
    (unless (eq (getf slot :allocation) :instance)
      (format s "~A  allocation: ~A~%" indent (getf slot :allocation)))))

(defun format-class-info (info)
  "Format class info plist as human-readable string."
  (with-output-to-string (s)
    (format s "~A::~A [~A]~%"
            (or (getf info :package) "#")
            (getf info :name)
            (getf info :metaclass))
    (when (getf info :documentation)
      (format s "~%~A~%~%" (getf info :documentation)))
    (format s "~%Superclasses: ~{~A~^, ~}~%"
            (or (getf info :superclasses) '("(none)")))
    (when (getf info :subclasses)
      (format s "Subclasses: ~{~A~^, ~}~%" (getf info :subclasses)))
    (let ((direct-slots (getf info :direct-slots)))
      (if direct-slots
          (progn
            (format s "~%Direct Slots (~D):~%" (length direct-slots))
            (dolist (slot direct-slots)
              (write-string (format-slot-info slot) s)))
          (format s "~%No direct slots~%")))
    (let ((inherited-slots (set-difference
                            (getf info :effective-slots)
                            (getf info :direct-slots)
                            :key (lambda (sl) (getf sl :name)))))
      (when inherited-slots
        (format s "~%Inherited Slots (~D):~%" (length inherited-slots))
        (dolist (slot inherited-slots)
          (format s "  ~A~@[ : ~S~]~%"
                  (getf slot :name)
                  (getf slot :type)))))
    (when (getf info :default-initargs)
      (format s "~%Default Initargs:~%")
      (dolist (ia (getf info :default-initargs))
        (format s "  ~A = ~A~%" (getf ia :key) (getf ia :value))))))

;;; ==========================================================================
;;; D.2: find-methods - Find Methods Specialized on a Class
;;; ==========================================================================

(defun introspect-method (method)
  "Extract information from a method object.
Returns a plist with :generic-function, :qualifiers, :lambda-list, :specializers, :documentation."
  (let ((gf (sb-mop:method-generic-function method)))
    (list :generic-function (when gf
                              (let ((name (sb-mop:generic-function-name gf)))
                                (if (symbolp name)
                                    (symbol-name name)
                                    (prin1-to-string name))))
          :qualifiers (sb-mop:method-qualifiers method)
          :lambda-list (sb-mop:method-lambda-list method)
          :specializers (mapcar (lambda (spec)
                                  (cond ((typep spec 'class)
                                         (class-name spec))
                                        ((typep spec 'sb-mop:eql-specializer)
                                         (list 'eql (sb-mop:eql-specializer-object spec)))
                                        (t spec)))
                                (sb-mop:method-specializers method))
          :documentation (documentation method t))))

(defun introspect-find-methods (class-designator &key include-inherited)
  "Find all methods specialized on a class.
CLASS-DESIGNATOR can be a class object, symbol, or string.
If INCLUDE-INHERITED is true, also include methods from superclasses.
Returns a list of method info plists."
  (let* ((class (etypecase class-designator
                  (class class-designator)
                  (symbol (or (find-class class-designator nil)
                              (error "Class ~A not found" class-designator)))
                  (string (let ((sym (find-symbol (string-upcase class-designator))))
                            (if sym
                                (or (find-class sym nil)
                                    (error "~A is not a class" class-designator))
                                (error "Symbol ~A not found" class-designator))))))
         (methods (sb-mop:specializer-direct-methods class)))
    (when include-inherited
      (dolist (super (rest (sb-mop:class-precedence-list class)))
        (setf methods (append methods (sb-mop:specializer-direct-methods super)))))
    ;; Remove duplicates and sort by generic function name
    (let ((unique-methods (remove-duplicates methods)))
      (sort (mapcar #'introspect-method unique-methods)
            #'string<
            :key (lambda (m) (or (getf m :generic-function) ""))))))

(defun format-method-info (method-info)
  "Format a method info plist as human-readable string."
  (with-output-to-string (s)
    (format s "  ~A"
            (or (getf method-info :generic-function) "(anonymous)"))
    (when (getf method-info :qualifiers)
      (format s " ~{~A~^ ~}" (getf method-info :qualifiers)))
    (format s " (~{~A~^, ~})~%"
            (getf method-info :specializers))))

(defun format-find-methods-results (results class-name)
  "Format find-methods results as human-readable string."
  (with-output-to-string (s)
    (if results
        (progn
          (format s "~D method~:P specialized on ~A:~%~%"
                  (length results) class-name)
          (dolist (m results)
            (write-string (format-method-info m) s)))
        (format s "No methods specialized on ~A~%" class-name))))

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
