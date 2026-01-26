;;; src/asdf-tools.lisp
;;; ABOUTME: ASDF and Quicklisp integration tools for system inspection and loading

(in-package #:cl-mcp-server.asdf-tools)

;;; ==========================================================================
;;; Dependency Normalization
;;; ==========================================================================

(defun normalize-dependency (dep)
  "Extract system name from a dependency specification.
Handles strings, symbols, and feature expressions like (:feature :foo (:require \"bar\"))."
  (cond
    ((stringp dep) dep)
    ((symbolp dep) (string-downcase (symbol-name dep)))
    ((and (consp dep) (eq (car dep) :feature))
     ;; (:feature :test (:require "name"))
     (let ((req (third dep)))
       (when (and (consp req) (member (car req) '(:require :version)))
         (normalize-dependency (second req)))))
    ((and (consp dep) (eq (car dep) :version))
     ;; (:version "name" "version")
     (normalize-dependency (second dep)))
    ((and (consp dep) (eq (car dep) :require))
     ;; (:require "name")
     (normalize-dependency (second dep)))
    (t nil)))

;;; ==========================================================================
;;; E.1: describe-system - ASDF System Information
;;; ==========================================================================

(defun collect-components (component)
  "Recursively collect all components from COMPONENT."
  (let ((children (when (typep component 'asdf:module)
                    (asdf:component-children component))))
    (cons (list :name (asdf:component-name component)
                :type (cond ((typep component 'asdf:cl-source-file) :file)
                            ((typep component 'asdf:module) :module)
                            ((typep component 'asdf:system) :system)
                            (t :other))
                :pathname (when (asdf:component-pathname component)
                            (namestring (asdf:component-pathname component))))
          (mapcan #'collect-components children))))

(defun introspect-system (system-name)
  "Get comprehensive information about an ASDF system.
Returns a plist with:
  :name - system name
  :version - version string
  :description - system description
  :author - author info
  :license - license info
  :pathname - source directory
  :source-file - .asd file path
  :components - list of component info
  :dependencies - direct dependencies"
  (let ((sys (asdf:find-system system-name nil)))
    (unless sys
      (error "System ~A not found" system-name))
    (list :name (asdf:component-name sys)
          :version (asdf:component-version sys)
          :description (asdf:system-description sys)
          :author (asdf:system-author sys)
          :maintainer (asdf:system-maintainer sys)
          :license (asdf:system-license sys)
          :homepage (asdf:system-homepage sys)
          :pathname (when (asdf:system-source-directory sys)
                      (namestring (asdf:system-source-directory sys)))
          :source-file (when (asdf:system-source-file sys)
                         (namestring (asdf:system-source-file sys)))
          :components (collect-components sys)
          :dependencies (mapcar #'normalize-dependency
                                (asdf:system-depends-on sys)))))

(defun format-system-info (info)
  "Format system info plist as human-readable string."
  (with-output-to-string (s)
    (format s "System: ~A~@[ (~A)~]~%"
            (getf info :name)
            (getf info :version))
    (format s "~%")
    (when (getf info :description)
      (format s "~A~%~%" (getf info :description)))
    (when (getf info :author)
      (format s "Author: ~A~%" (getf info :author)))
    (when (getf info :maintainer)
      (format s "Maintainer: ~A~%" (getf info :maintainer)))
    (when (getf info :license)
      (format s "License: ~A~%" (getf info :license)))
    (when (getf info :homepage)
      (format s "Homepage: ~A~%" (getf info :homepage)))
    (format s "~%Source: ~A~%"
            (or (getf info :source-file) (getf info :pathname)))
    (let ((deps (getf info :dependencies)))
      (if deps
          (format s "~%Dependencies (~D): ~{~A~^, ~}~%"
                  (length deps) deps)
          (format s "~%No dependencies~%")))
    (let ((components (remove-if (lambda (c) (eq (getf c :type) :system))
                                 (getf info :components))))
      (when components
        (format s "~%Components (~D):~%" (length components))
        (dolist (c components)
          (format s "  ~A [~A]~%" (getf c :name) (getf c :type)))))))

;;; ==========================================================================
;;; E.4: system-dependencies - Dependency Graph
;;; ==========================================================================

(defun introspect-system-dependencies (system-name &key transitive)
  "Get dependencies of SYSTEM-NAME.
If TRANSITIVE is true, include all transitive dependencies.
Returns a plist with :system, :direct, and optionally :transitive."
  (let ((sys (asdf:find-system system-name nil)))
    (unless sys
      (error "System ~A not found" system-name))
    (let ((direct (mapcar #'normalize-dependency (asdf:system-depends-on sys)))
          (all-deps nil))
      (when transitive
        (labels ((collect-deps (s)
                   (let ((deps (asdf:system-depends-on s)))
                     (dolist (dep deps)
                       (let ((dep-name (normalize-dependency dep)))
                         (when (and dep-name
                                    (not (member dep-name all-deps :test #'string-equal)))
                           (push dep-name all-deps)
                           (let ((dep-sys (asdf:find-system dep-name nil)))
                             (when dep-sys
                               (collect-deps dep-sys)))))))))
          (collect-deps sys)))
      (list :system (asdf:component-name sys)
            :direct (remove nil direct)
            :transitive (when transitive (nreverse all-deps))))))

(defun format-system-dependencies (info)
  "Format system dependencies as human-readable string."
  (with-output-to-string (s)
    (format s "Dependencies for ~A:~%~%" (getf info :system))
    (let ((direct (getf info :direct)))
      (if direct
          (progn
            (format s "Direct (~D):~%" (length direct))
            (dolist (d direct)
              (format s "  ~A~%" d)))
          (format s "No direct dependencies~%")))
    (let ((transitive (getf info :transitive)))
      (when transitive
        (format s "~%All (~D):~%" (length transitive))
        (dolist (d transitive)
          (format s "  ~A~%" d))))))

;;; ==========================================================================
;;; E.5: list-local-systems - Local System Discovery
;;; ==========================================================================

(defun introspect-local-systems ()
  "List all ASDF systems findable from current configuration.
Returns list of plists with :name and :pathname."
  (let ((results nil))
    ;; Get systems from Quicklisp local projects if available
    (when (find-package :ql)
      (dolist (name (funcall (find-symbol "LIST-LOCAL-SYSTEMS" :ql)))
        (let ((sys (asdf:find-system name nil)))
          (when sys
            (push (list :name name
                        :pathname (when (asdf:system-source-file sys)
                                    (namestring (asdf:system-source-file sys))))
                  results)))))
    ;; Also check ASDF source registry
    (dolist (sys-name (asdf:registered-systems))
      (unless (find sys-name results :key (lambda (r) (getf r :name)) :test #'string-equal)
        (let ((sys (asdf:find-system sys-name nil)))
          (when sys
            (push (list :name sys-name
                        :pathname (when (asdf:system-source-file sys)
                                    (namestring (asdf:system-source-file sys))))
                  results)))))
    (sort results #'string< :key (lambda (r) (getf r :name)))))

(defun format-local-systems (systems)
  "Format local systems list as human-readable string."
  (with-output-to-string (s)
    (format s "Local Systems (~D):~%~%" (length systems))
    (dolist (sys systems)
      (format s "  ~A~%" (getf sys :name))
      (when (getf sys :pathname)
        (format s "    ~A~%" (getf sys :pathname))))))

(defun introspect-find-system-file (system-name)
  "Find the .asd file for a system.
Returns a plist with :found, :name, :pathname."
  (let ((sys (asdf:find-system system-name nil)))
    (if sys
        (list :found t
              :name (asdf:component-name sys)
              :pathname (when (asdf:system-source-file sys)
                          (namestring (asdf:system-source-file sys))))
        (list :found nil
              :name system-name
              :pathname nil))))

;;; ==========================================================================
;;; E.3: quickload / quicklisp-search - Quicklisp Integration
;;; ==========================================================================

(defun quicklisp-available-p ()
  "Check if Quicklisp is available."
  (and (find-package :ql) t))

(defun introspect-quickload (system-name &key verbose)
  "Load a system via Quicklisp, downloading if necessary.
Returns a plist with :loaded, :system, :downloaded (if any new systems were fetched)."
  (unless (quicklisp-available-p)
    (error "Quicklisp is not available"))
  (let* ((ql-quickload (find-symbol "QUICKLOAD" :ql))
         (before-systems (asdf:registered-systems))
         (warnings nil)
         result)
    (handler-bind
        ((warning (lambda (w)
                    (push (format nil "~A" w) warnings)
                    (muffle-warning w))))
      (funcall ql-quickload system-name :verbose verbose :silent (not verbose)))
    (let ((after-systems (asdf:registered-systems)))
      (setf result
            (list :loaded t
                  :system (string-downcase (string system-name))
                  :newly-registered (set-difference after-systems before-systems
                                                    :test #'string-equal)
                  :warnings (nreverse warnings))))
    result))

(defun format-quickload-result (info)
  "Format quickload result as human-readable string."
  (with-output-to-string (s)
    (format s "~A loaded successfully.~%"
            (getf info :system))
    (let ((new (getf info :newly-registered)))
      (when new
        (format s "~%Newly registered systems (~D):~%" (length new))
        (dolist (n new)
          (format s "  ~A~%" n))))
    (let ((warnings (getf info :warnings)))
      (when warnings
        (format s "~%Warnings (~D):~%" (length warnings))
        (dolist (w warnings)
          (format s "  ~A~%" w))))))

(defun introspect-quicklisp-search (pattern &key (limit 30))
  "Search Quicklisp for systems matching PATTERN.
Returns list of matching system names."
  (unless (quicklisp-available-p)
    (error "Quicklisp is not available"))
  (let ((ql-system-list (find-symbol "SYSTEM-LIST" :ql))
        (ql-dist-name (find-symbol "NAME" :ql-dist))
        (results nil)
        (count 0))
    (dolist (sys (funcall ql-system-list))
      (when (>= count limit)
        (return))
      (let ((name (funcall ql-dist-name sys)))
        (when (search pattern name :test #'char-equal)
          (push name results)
          (incf count))))
    (sort (nreverse results) #'string<)))

(defun format-quicklisp-search-results (results pattern)
  "Format Quicklisp search results as human-readable string."
  (with-output-to-string (s)
    (if results
        (progn
          (format s "Found ~D system~:P matching '~A':~%~%"
                  (length results) pattern)
          (dolist (r results)
            (format s "  ~A~%" r)))
        (format s "No systems found matching '~A'~%" pattern))))

;;; ==========================================================================
;;; E.6: load-file - Load Single Lisp File
;;; ==========================================================================

(defun introspect-load-file (pathname &key compile (package "CL-USER"))
  "Load a single Lisp file into the image.
If COMPILE is true, compile before loading.
PACKAGE sets the *package* during loading.
Returns a plist with :loaded, :pathname, :warnings."
  (let ((path (pathname pathname))
        (warnings nil)
        (pkg (or (find-package (string-upcase package))
                 (error "Package ~A not found" package))))
    (unless (probe-file path)
      (error "File not found: ~A" pathname))
    (let ((*package* pkg))
      (handler-bind
          ((warning (lambda (w)
                      (push (format nil "~A" w) warnings)
                      (muffle-warning w))))
        (if compile
            (let ((fasl (compile-file path)))
              (when fasl
                (load fasl)))
            (load path))))
    (list :loaded t
          :pathname (namestring (truename path))
          :compiled compile
          :package (package-name pkg)
          :warnings (nreverse warnings))))

(defun format-load-file-result (info)
  "Format load-file result as human-readable string."
  (with-output-to-string (s)
    (format s "Loaded: ~A~%" (getf info :pathname))
    (when (getf info :compiled)
      (format s "  (compiled before loading)~%"))
    (format s "  in package: ~A~%" (getf info :package))
    (let ((warnings (getf info :warnings)))
      (if warnings
          (progn
            (format s "~%Warnings (~D):~%" (length warnings))
            (dolist (w warnings)
              (format s "  ~A~%" w)))
          (format s "No warnings.~%")))))
