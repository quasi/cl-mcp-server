# Troubleshooting Common Issues

**Problem**: Something isn't working as expected with CL-MCP-Server.

This guide helps you diagnose and fix common problems.

## Prerequisites

- Basic familiarity with CL-MCP-Server
- Access to terminal for running diagnostic commands

---

## Server Won't Start

### Symptom

Claude Code reports "Failed to connect to MCP server" or server process exits immediately.

### Diagnosis

Run the server manually to see error messages:

```bash
sbcl --load run-server.lisp
```

### Common Causes

**1. SBCL Not Installed**

```
Error: sbcl: command not found
```

**Fix**: Install SBCL:
- macOS: `brew install sbcl`
- Ubuntu/Debian: `apt-get install sbcl`
- Windows: Download from [sbcl.org](http://www.sbcl.org/)

**2. Quicklisp Not Installed**

```
Error: Cannot find Quicklisp
```

**Fix**: Install Quicklisp:
```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
```

**3. Dependencies Missing**

```
Error: System "yason" not found
```

**Fix**: Load dependencies:
```bash
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server)" \
     --quit
```

**4. Wrong Path in Configuration**

Check your `~/.claude/mcp_config.json`:

```json
{
  "mcpServers": {
    "lisp": {
      "command": "sbcl",
      "args": [
        "--load", "/FULL/PATH/TO/cl-mcp-server/run-server.lisp"
      ]
    }
  }
}
```

**Fix**: Use absolute paths, not relative paths like `~/` or `./`.

---

## Evaluations Fail Silently

### Symptom

Claude doesn't respond to evaluation requests or returns empty results.

### Diagnosis

Check if the server is receiving requests by looking at stderr output.

### Common Causes

**1. Server Process Died**

**Fix**: Restart Claude Code to restart the server.

**2. JSON-RPC Protocol Error**

The server logs protocol errors to stderr. Check Claude Code logs:

```bash
# Location varies by platform
# macOS: ~/Library/Logs/Claude/
# Linux: ~/.config/Claude/logs/
```

---

## Wrong Results or Unexpected Behavior

### Symptom

Code evaluates but produces unexpected results.

### Diagnosis

**1. Check Current Package**

```lisp
*package*
;; => #<PACKAGE "SOME-PACKAGE">

;; Return to CL-USER if needed
(in-package :cl-user)
```

**2. Check for Shadowed Symbols**

```lisp
;; See if a symbol is shadowed
(find-symbol "FUNCTION-NAME")

;; List all symbols in current package
(do-symbols (s *package*) (print s))
```

**3. Check Loaded Systems**

```lisp
;; See what systems are loaded
(remove-if-not #'asdf:component-loaded-p
               (asdf:registered-systems))
```

### Common Causes

**1. Wrong Package Context**

Functions defined in one package aren't visible in another.

**Fix**: Either:
- Switch to the package: `(in-package :my-package)`
- Use qualified names: `my-package:function-name`

**2. Redefined Standard Functions**

```lisp
;; Accidentally redefining CL functions
(defun + (x y) (* x y))  ; BAD!

;; Now + is multiplication
(+ 2 3)  ;; => 6 (should be 5)
```

**Fix**: Reset session (see below) and avoid redefining standard library functions.

**3. State from Previous Evaluations**

Session state persists, which can cause confusion:

```lisp
;; First evaluation
(defparameter *x* 10)

;; Much later...
(+ *x* 5)  ;; => 15
;; Might be confusing if you forgot *x* was defined
```

**Fix**: Check your defined variables:
```lisp
(apropos "*" *package*)
```

---

## Session State Issues

### Symptom

Session has accumulated too much state or has conflicting definitions.

### Solution 1: Reset Session (When Available)

Once the `reset-session` tool is implemented:

```
User: Reset the session
Claude: [calls reset-session tool]
        Session reset. All definitions cleared.
        Current package: CL-USER
```

### Solution 2: Restart Server

Restart Claude Code to get a fresh session.

### Solution 3: Selective Cleanup

```lisp
;; Undefine a function
(fmakunbound 'my-function)

;; Unbind a variable
(makunbound '*my-var*)

;; Remove a package
(delete-package :my-package)
```

---

## Performance Issues

### Symptom

Evaluations take a long time or hang.

### Common Causes

**1. Infinite Loop**

```lisp
(defun infinite ()
  (infinite))  ; Never terminates

(infinite)  ; Hangs forever
```

**Fix**: Currently, you'll need to restart the server. Future versions may add timeout support.

**2. Large Data Structures**

```lisp
;; Creating huge lists
(make-list 10000000)  ; Very slow
```

**Fix**: Use smaller test data or more efficient data structures (arrays, hash tables).

**3. Expensive Computation**

```lisp
;; Factorial of large numbers
(defun factorial (n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(factorial 100000)  ; Very slow
```

**Fix**: Optimize algorithms or use memoization.

---

## Output Not Appearing

### Symptom

You print something but don't see the output.

### Diagnosis

```lisp
(print "Hello")
;; Output should appear in [stdout] section
```

### Common Causes

**1. Using format with nil stream**

```lisp
(format nil "Hello ~A" "World")
;; => "Hello World"
;; Returns string, doesn't print

;; To print to stdout:
(format t "Hello ~A" "World")
```

**2. Output Captured in Variable**

```lisp
(let ((output (with-output-to-string (*standard-output*)
                (print "Hello"))))
  output)
;; Output captured in string, not printed to stdout
```

---

## Error Messages Are Unclear

### Symptom

Error message doesn't help identify the problem.

### Solution: Get More Information

**1. Examine the Backtrace**

Error responses include backtraces showing the call stack:

```
[ERROR] DIVISION-BY-ZERO
arithmetic error DIVISION-BY-ZERO signalled

[Backtrace]
0: (/ 1 0)
1: (CALCULATE-AVERAGE NIL)
2: (PROCESS-DATA ...)
```

**2. Use describe for Type Errors**

```lisp
(handler-case
    (+ 1 "hello")
  (type-error (c)
    (format t "Expected: ~A~%" (type-error-expected-type c))
    (format t "Got: ~A~%" (type-error-datum c))
    (format t "Full details: ~A~%" (describe c))))
```

**3. Check Condition Slots**

```lisp
(handler-case
    (error-producing-code)
  (error (c)
    ;; Print all condition info
    (describe c)
    (format t "Type: ~A~%" (type-of c))
    :error))
```

---

## JSON-RPC Errors

### Symptom

"Invalid JSON-RPC request" or protocol-level errors.

### Cause

This usually means the MCP client (Claude Code) sent a malformed request, or there's a version mismatch.

### Solution

1. **Update Claude Code** to the latest version
2. **Check server version** matches MCP protocol version
3. **Report the issue** with the exact error message

---

## Advanced Diagnostics

### Enable Debug Output (Future Feature)

```lisp
(setf *debug-mode* t)

;; See verbose protocol messages
```

### Check Server Health

```lisp
;; Verify server is responding
(+ 1 1)  ;; => 2 (if this works, server is healthy)

;; Check available tools
;; (Future: list-tools command)
```

### Inspect Session State

```lisp
;; See all defined functions in current package
(do-symbols (s *package*)
  (when (and (fboundp s)
             (eq (symbol-package s) *package*))
    (print s)))

;; See all defined variables
(do-symbols (s *package*)
  (when (and (boundp s)
             (eq (symbol-package s) *package*))
    (print s)))
```

---

## Getting Help

If you're still stuck:

1. **Check the logs**: Look in Claude Code logs for detailed error messages
2. **Reproduce manually**: Try running the code directly in SBCL to isolate the issue
3. **Simplify**: Create a minimal example that demonstrates the problem
4. **Report**: Open an issue with:
   - Exact steps to reproduce
   - Expected vs actual behavior
   - Error messages (full text)
   - Your environment (OS, SBCL version, Quicklisp version)

## Prevention Tips

1. **Start Fresh**: When working on a new problem, consider resetting the session
2. **Use Packages**: Organize code in separate packages to avoid naming conflicts
3. **Test Incrementally**: Define and test functions one at a time
4. **Document State**: Keep track of what you've defined in the session
5. **Use Version Control**: For complex work, save code to files rather than relying solely on session state

## See Also

- [Error Handling Guide](handle-errors-gracefully.md) - Writing robust error-handling code
- [Session State Contract](../../canon/features/session-management/contracts/session-state.md) - What persists and what doesn't
- [Architecture Explanation](../explanation/architecture.md) - How the server works internally
- [Quickstart](../quickstart.md) - Setting up from scratch
