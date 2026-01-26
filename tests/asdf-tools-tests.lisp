;;; tests/asdf-tools-tests.lisp
;;; ABOUTME: Tests for Phase E ASDF and Quicklisp integration tools

(in-package #:cl-mcp-server-tests)

(def-suite asdf-tools-tests
  :description "Tests for ASDF and Quicklisp integration tools"
  :in cl-mcp-server-tests)

(in-suite asdf-tools-tests)

;;; ==========================================================================
;;; Dependency Normalization Tests
;;; ==========================================================================

(test normalize-dependency-string
  "normalize-dependency handles string dependencies"
  (is (string= "foo" (cl-mcp-server.asdf-tools:normalize-dependency "foo"))))

(test normalize-dependency-symbol
  "normalize-dependency handles symbol dependencies"
  (is (string= "foo" (cl-mcp-server.asdf-tools:normalize-dependency :foo)))
  (is (string= "bar" (cl-mcp-server.asdf-tools:normalize-dependency 'bar))))

(test normalize-dependency-version
  "normalize-dependency handles version specifications"
  (is (string= "foo"
               (cl-mcp-server.asdf-tools:normalize-dependency '(:version "foo" "1.0")))))

(test normalize-dependency-feature
  "normalize-dependency handles feature expressions"
  (is (string= "threads"
               (cl-mcp-server.asdf-tools:normalize-dependency
                '(:feature :sbcl (:require "threads"))))))

;;; ==========================================================================
;;; describe-system Tests (E.1)
;;; ==========================================================================

(test introspect-system-basic
  "introspect-system returns system info"
  (let ((info (cl-mcp-server.asdf-tools:introspect-system "cl-mcp-server")))
    (is (string= "cl-mcp-server" (getf info :name)))
    (is (not (null (getf info :version))))
    (is (not (null (getf info :description))))
    (is (not (null (getf info :dependencies))))))

(test introspect-system-components
  "introspect-system includes components"
  (let* ((info (cl-mcp-server.asdf-tools:introspect-system "cl-mcp-server"))
         (components (getf info :components)))
    (is (not (null components)))
    (is (find "tools" components :key (lambda (c) (getf c :name)) :test #'string=))))

(test introspect-system-not-found
  "introspect-system signals error for nonexistent system"
  (signals error
    (cl-mcp-server.asdf-tools:introspect-system "nonexistent-system-xyz")))

(test format-system-info-output
  "format-system-info produces readable output"
  (let* ((info (cl-mcp-server.asdf-tools:introspect-system "cl-mcp-server"))
         (formatted (cl-mcp-server.asdf-tools:format-system-info info)))
    (is (search "cl-mcp-server" formatted))
    (is (search "Dependencies" formatted))))

;;; ==========================================================================
;;; system-dependencies Tests (E.4)
;;; ==========================================================================

(test introspect-system-dependencies-direct
  "introspect-system-dependencies returns direct dependencies"
  (let ((info (cl-mcp-server.asdf-tools:introspect-system-dependencies "cl-mcp-server")))
    (is (string= "cl-mcp-server" (getf info :system)))
    (is (not (null (getf info :direct))))
    (is (member "yason" (getf info :direct) :test #'string=))))

(test introspect-system-dependencies-transitive
  "introspect-system-dependencies returns transitive dependencies"
  (let ((info (cl-mcp-server.asdf-tools:introspect-system-dependencies
               "cl-mcp-server" :transitive t)))
    (is (not (null (getf info :transitive))))
    ;; Transitive should include more than direct
    (is (>= (length (getf info :transitive))
            (length (getf info :direct))))))

(test format-system-dependencies-output
  "format-system-dependencies produces readable output"
  (let* ((info (cl-mcp-server.asdf-tools:introspect-system-dependencies "cl-mcp-server"))
         (formatted (cl-mcp-server.asdf-tools:format-system-dependencies info)))
    (is (search "cl-mcp-server" formatted))
    (is (search "Direct" formatted))))

;;; ==========================================================================
;;; list-local-systems Tests (E.5)
;;; ==========================================================================

(test introspect-local-systems-returns-list
  "introspect-local-systems returns a list of systems"
  (let ((systems (cl-mcp-server.asdf-tools:introspect-local-systems)))
    (is (listp systems))
    (is (not (null systems)))
    ;; Should include cl-mcp-server
    (is (find "cl-mcp-server" systems
              :key (lambda (s) (getf s :name))
              :test #'string=))))

(test introspect-local-systems-has-pathnames
  "introspect-local-systems includes pathnames"
  (let* ((systems (cl-mcp-server.asdf-tools:introspect-local-systems))
         (our-sys (find "cl-mcp-server" systems
                        :key (lambda (s) (getf s :name))
                        :test #'string=)))
    (is (not (null (getf our-sys :pathname))))))

(test format-local-systems-output
  "format-local-systems produces readable output"
  (let* ((systems (cl-mcp-server.asdf-tools:introspect-local-systems))
         (formatted (cl-mcp-server.asdf-tools:format-local-systems systems)))
    (is (search "Local Systems" formatted))))

;;; ==========================================================================
;;; find-system-file Tests (E.5)
;;; ==========================================================================

(test introspect-find-system-file-found
  "introspect-find-system-file finds existing systems"
  (let ((result (cl-mcp-server.asdf-tools:introspect-find-system-file "cl-mcp-server")))
    (is (getf result :found))
    (is (string= "cl-mcp-server" (getf result :name)))
    (is (search ".asd" (getf result :pathname)))))

(test introspect-find-system-file-not-found
  "introspect-find-system-file returns :found nil for missing systems"
  (let ((result (cl-mcp-server.asdf-tools:introspect-find-system-file
                 "nonexistent-system-xyz")))
    (is (null (getf result :found)))))

;;; ==========================================================================
;;; Quicklisp Integration Tests (E.3)
;;; ==========================================================================

(test quicklisp-available-p
  "quicklisp-available-p returns true when Quicklisp is loaded"
  ;; Quicklisp should be available in test environment
  (is (cl-mcp-server.asdf-tools:quicklisp-available-p)))

(test introspect-quicklisp-search-finds-systems
  "introspect-quicklisp-search finds matching systems"
  (let ((results (cl-mcp-server.asdf-tools:introspect-quicklisp-search "json" :limit 5)))
    (is (listp results))
    (is (<= (length results) 5))
    ;; All results should contain "json"
    (is (every (lambda (name) (search "json" name :test #'char-equal)) results))))

(test introspect-quicklisp-search-respects-limit
  "introspect-quicklisp-search respects limit parameter"
  (let ((results (cl-mcp-server.asdf-tools:introspect-quicklisp-search "cl" :limit 3)))
    (is (<= (length results) 3))))

(test format-quicklisp-search-results-output
  "format-quicklisp-search-results produces readable output"
  (let* ((results (cl-mcp-server.asdf-tools:introspect-quicklisp-search "json" :limit 3))
         (formatted (cl-mcp-server.asdf-tools:format-quicklisp-search-results
                     results "json")))
    (is (search "json" formatted))))

;;; ==========================================================================
;;; load-file Tests (E.6)
;;; ==========================================================================

(test introspect-load-file-not-found
  "introspect-load-file signals error for missing files"
  (signals error
    (cl-mcp-server.asdf-tools:introspect-load-file "/nonexistent/file.lisp")))

(test introspect-load-file-invalid-package
  "introspect-load-file signals error for invalid package"
  (signals error
    (cl-mcp-server.asdf-tools:introspect-load-file
     "/tmp/test.lisp" :package "NONEXISTENT-PACKAGE-XYZ")))

;;; ==========================================================================
;;; collect-components Tests
;;; ==========================================================================

(test collect-components-recursive
  "collect-components recursively collects all components"
  (let* ((sys (asdf:find-system "cl-mcp-server"))
         (components (cl-mcp-server.asdf-tools:collect-components sys)))
    (is (not (null components)))
    ;; Should find nested file components
    (is (find "tools" components :key (lambda (c) (getf c :name)) :test #'string=))))
