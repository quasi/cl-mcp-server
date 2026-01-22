# Tutorial: Your First REPL Session

**What you'll learn**: How to interact with the CL-MCP-Server using Claude to build up a working program incrementally.

**Time**: 15 minutes

## Prerequisites

- Server running and connected to Claude Code
- Basic familiarity with Common Lisp syntax (helpful but not required)

## Goal

Build a simple temperature converter that can convert between Celsius and Fahrenheit, testing it as we go.

## Step 1: Test the Connection

First, verify the server is working:

**You**: "Please evaluate `(+ 1 1)` in Lisp"

**Expected response**:
```
=> 2
```

✓ **Checkpoint**: If you see `=> 2`, your server is working!

## Step 2: Define the Celsius to Fahrenheit Function

**You**: "Please evaluate this Lisp code: `(defun c-to-f (celsius) (* (+ celsius 32) 9/5))`"

Wait, that formula is wrong! Let's fix it:

**You**: "Please evaluate: `(defun c-to-f (celsius) (+ (* celsius 9/5) 32))`"

**Expected response**:
```
=> C-TO-F
```

✓ **Checkpoint**: The function is now defined and available in the session.

## Step 3: Test the Function

**You**: "Please evaluate `(c-to-f 0)`"

**Expected response**:
```
=> 32
```

Water freezes at 0°C = 32°F. Perfect!

**You**: "Please evaluate `(c-to-f 100)`"

**Expected response**:
```
=> 212
```

Water boils at 100°C = 212°F. Our function works!

✓ **Checkpoint**: The function we defined earlier is still available.

## Step 4: Define the Reverse Function

**You**: "Please evaluate: `(defun f-to-c (fahrenheit) (/ (* (- fahrenheit 32) 5) 9))`"

**Expected response**:
```
=> F-TO-C
```

## Step 5: Test the Reverse Function

**You**: "Please evaluate `(f-to-c 32)`"

**Expected response**:
```
=> 0
```

**You**: "Please evaluate `(f-to-c 212)`"

**Expected response**:
```
=> 100
```

✓ **Checkpoint**: Both conversion functions work correctly.

## Step 6: Use Both Functions Together

**You**: "Please evaluate: `(f-to-c (c-to-f 25))`"

**Expected response**:
```
=> 25
```

Converting 25°C to Fahrenheit and back to Celsius gives us 25°C. Round trip works!

## Step 7: Add a Formatting Function

Let's make the output more user-friendly:

**You**: "Please evaluate this code:"

```lisp
(defun format-temp (value unit)
  (format nil "~,1F°~A" value unit))
```

**Expected response**:
```
=> FORMAT-TEMP
```

## Step 8: Test Formatting

**You**: "Please evaluate: `(format-temp 25.5 "C")`"

**Expected response**:
```
=> "25.5°C"
```

**You**: "Please evaluate: `(format-temp (c-to-f 25) "F")`"

**Expected response**:
```
=> "77.0°F"
```

## Step 9: Build a Conversion Table

Now let's use all our functions together:

**You**: "Please evaluate this code:"

```lisp
(loop for c from 0 to 100 by 10
      collect (list (format-temp c "C")
                    (format-temp (c-to-f c) "F")))
```

**Expected response**:
```
=> (("0.0°C" "32.0°F")
    ("10.0°C" "50.0°F")
    ("20.0°C" "68.0°F")
    ("30.0°C" "86.0°F")
    ("40.0°C" "104.0°F")
    ("50.0°C" "122.0°F")
    ("60.0°C" "140.0°F")
    ("70.0°C" "158.0°F")
    ("80.0°C" "176.0°F")
    ("90.0°C" "194.0°F")
    ("100.0°C" "212.0°F"))
```

✓ **Final Checkpoint**: You've built a complete temperature conversion system incrementally!

## What You Learned

1. **Persistent State**: Functions you define remain available in later evaluations
2. **Incremental Development**: You can define and test functions one at a time
3. **Error Recovery**: You can redefine functions if you make a mistake
4. **Composition**: Functions can call other functions you've defined
5. **Rich Output**: The server handles complex return values (lists, formatted strings)

## What's Different from a Regular REPL?

| REPL | CL-MCP-Server |
|------|---------------|
| Interactive prompts | Request/response through Claude |
| Direct keyboard input | Natural language requests |
| Immediate visual feedback | Structured JSON responses |
| Local terminal | Works over stdio transport |

## Next Steps

- **Learn error handling**: See [Tutorial: Handling Errors](02-error-handling.md)
- **Work with output**: See [Tutorial: Capturing Output](03-output-capture.md)
- **Understand packages**: See [How-To: Switch Packages](../how-to/switch-packages.md)

## Try It Yourself

Build a simple calculator that:
1. Has functions for add, subtract, multiply, divide
2. Has a function that takes an operator symbol and two numbers
3. Can handle a list of operations

Hints:
- Use `defun` to define functions
- Use `case` or `cond` to dispatch on operator symbols
- Use `mapcar` to process lists

---

**Congratulations!** You've completed your first REPL session with CL-MCP-Server.
