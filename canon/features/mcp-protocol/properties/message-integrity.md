---
type: property
name: message-integrity
version: 1.0.0
feature: mcp-protocol
covers:
  - contracts/transport
---

# Message Integrity Property

## Statement

**For all** messages sent by the server,
**the message** MUST be well-formed, complete JSON terminated by newline,
parseable by any standard JSON parser.

## Formal Expression

```
∀ message ∈ ServerOutput :
  is_complete_json(message) ∧
  ends_with_newline(message) ∧
  json_parse(message) succeeds ∧
  is_single_object(json_parse(message))

where:
  is_complete_json(m) ≡ balanced_braces(m) ∧ valid_json_syntax(m)
  ends_with_newline(m) ≡ m[length(m)-1] = '\n'
```

## Informal Explanation

The MCP protocol uses newline-delimited JSON over stdio. Each message must be:

1. **Complete**: A full JSON object (not truncated)
2. **Well-formed**: Valid JSON syntax (balanced braces, quoted strings, etc.)
3. **Newline-terminated**: Ends with `\n` character
4. **Single object**: One JSON object per line (not multiple objects)
5. **Parseable**: Can be parsed by standard JSON libraries

This ensures the client can reliably split the stream into messages and parse each one.

## Rationale

Stdio transport requires clear message boundaries. Newline-delimited JSON is the standard approach. Incomplete or malformed messages would cause the client to hang, misparse, or lose synchronization with the message stream.

## Counterexample Shape

If this property is violated, you might see:
- Truncated JSON: `{"jsonrpc":"2.0","id":1,"result":{"val`
- Missing newline: `{"jsonrpc":"2.0","id":1,"result":{"value":42}}`
- Multiple objects on one line: `{"jsonrpc":"2.0"...}{"jsonrpc":"2.0"...}\n`
- Unbalanced braces: `{"jsonrpc":"2.0","id":1,"result":{"value":42}\n`
- Invalid JSON: `{jsonrpc:"2.0",id:1,result:{value:42}}\n`

## Verification Approach

**Output Validation**:

For every server response:
```lisp
(defun verify-message-integrity (server-output)
  ;; 1. Check newline termination
  (assert (ends-with-newline server-output))

  ;; 2. Parse as JSON
  (let ((parsed (handler-case (json:decode-json-from-string server-output)
                  (error () nil))))
    (assert parsed "Must be valid JSON")

    ;; 3. Ensure single object
    (assert (not (consp (car parsed))) "Should be single object, not array")))
```

**Stream Splitting Test**:
```lisp
;; Simulate stdio stream with multiple messages
(defun test-message-boundaries ()
  (let* ((msg1 (format-response response1))
         (msg2 (format-response response2))
         (stream (concatenate 'string msg1 msg2)))

    ;; Should split cleanly on newlines
    (let ((messages (split-by-newline stream)))
      (assert (= (length messages) 2))
      (assert (json-parseable (first messages)))
      (assert (json-parseable (second messages))))))
```

**Property Test**:
- Generate 1000 random responses
- Format each as JSON
- Verify each is parseable
- Verify each ends with newline
- Concatenate all into stream
- Verify stream splits cleanly into 1000 messages

**Shrinking**: Find minimal response that produces malformed output
