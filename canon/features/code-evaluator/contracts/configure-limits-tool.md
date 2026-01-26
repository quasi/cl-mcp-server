---
type: contract
name: configure-limits-tool
version: 0.1.0
---

# Configure-Limits Tool Contract

Tool for adjusting evaluation safety parameters at runtime.

## Tool Definition

```json
{
  "name": "configure-limits",
  "description": "Configure evaluation safety limits. Returns current configuration after any changes.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "timeout": {
        "type": "integer",
        "description": "Evaluation timeout in seconds (default: 30). Set to 0 to disable (not recommended)."
      },
      "max-output": {
        "type": "integer",
        "description": "Maximum output characters to capture (default: 100000)"
      }
    }
  }
}
```

## Parameters

### timeout (optional)

Maximum seconds allowed for evaluation before timeout.

| Value | Behavior |
|-------|----------|
| Positive integer | Set timeout to that many seconds |
| 0 | Disable timeout (WARNING: risks server hangs) |
| Omitted | No change to current setting |

### max-output (optional)

Maximum characters to capture from stdout/stderr.

| Value | Behavior |
|-------|----------|
| Positive integer | Set limit to that many characters |
| Omitted | No change to current setting |

## Output Format

Always returns current configuration after any changes:

```
Current limits:
  timeout: 30 seconds
  max-output: 100000 characters
```

When timeout is disabled:

```
Current limits:
  timeout: disabled seconds (WARNING: no timeout)
  max-output: 100000 characters
```

## Use Cases

### Increase Timeout for Long Operations

When loading large systems or running expensive computations:

```json
{"name": "configure-limits", "arguments": {"timeout": 120}}
```

### Query Current Settings

Call with no arguments to see current configuration:

```json
{"name": "configure-limits", "arguments": {}}
```

### Disable Timeout (Use with Caution)

For trusted code that needs unlimited time:

```json
{"name": "configure-limits", "arguments": {"timeout": 0}}
```

**Warning**: Disabling timeout means infinite loops will hang the server.
Only disable when you trust the code being evaluated.

## Scope

Changes apply globally to the server session. They persist until:
- Changed again via `configure-limits`
- Server is restarted (reverts to defaults)

## Related

- [evaluate-lisp-tool](evaluate-lisp-tool.md) - Uses these limits
- [Evaluation Timeout](../vocabulary.md#evaluation-timeout) - Vocabulary
