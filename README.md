# Shesh: A Shell Interface for Common Lisp

Shesh is a lightweight Common Lisp library that provides a simple and intuitive interface for interacting with shell processes.

## Features

- **Persistent Shell Sessions**: Maintain a single shell process across multiple commands
- **State Preservation**: Working directory and environment variables persist between commands
- **Simple API**: Easy-to-use functions and macros for common shell operations
- **Output Capture**: Automatically captures and stores command output
- **Variable Expansion**: Use Lisp variables in shell commands with `$` syntax
- **Smart Command Completion**: Accurately detects when commands finish execution
- **No Quotes Needed**: Write shell commands naturally without string quotes

## Installation

1. Clone this repository to your local machine
2. Load the system using ASDF:

```lisp
(asdf:load-system "shesh")
```

## Basic Usage

```lisp
(in-package :shesh)

;; Initialize a shell session
(init-shell)

;; Execute commands using the sh macro (no quotes needed!)
(sh pwd)
;; => "/home/user/projects"

(sh ls -la)
;; => "total 64\ndrwxr-xr-x  5 user user 4096 ..."

;; Use Lisp variables in shell commands
(let ((name "Alice")
      (count 5))
  (sh echo Hello $name, you have $count messages))
;; => "hello Alice, you have 5 messages"

;; Access the output
*stdout*
;; => "hello Alice, you have 5 messages"

;; Change directory (state is preserved)
(sh cd /tmp)
(sh pwd)
;; => "/tmp"

;; Close the shell when done
(close-shell)
```

## Variable Expansion

Shesh supports automatic variable expansion using the `$` prefix:

```lisp
;; Simple variable substitution
(let ((x 42))
  (sh echo The answer is $x))
;; => "the answer is 42"

;; Variables with spaces
(let ((message "Hello World"))
  (sh echo $message))
;; => "Hello World"

;; Multiple variables
(let ((user "Bob")
      (dir "/home"))
  (sh echo User $user home is $dir/$user))
;; => "user Bob home is /home/Bob"

;; Unbound variables are passed to the shell as-is
(sh echo $HOME)  ; Shell environment variable
;; => "/Users/username"
```

## API Reference

### Macros

- `(sh &rest command-parts)` - Execute a shell command without quotes
  - Supports variable expansion with `$` prefix
  - Example: `(sh echo hello world)`

### Functions

- `(init-shell &optional shell-command)` - Initialize a new shell process
- `(exec command)` - Execute a command string in the current shell
- `(stdout)` - Manually consume available output from the shell
- `(close-shell)` - Close the current shell process

### Special Variables

- `*current-shell*` - The current shell process object
- `*stdout*` - Buffer containing the shell output

## Examples

See `example.lisp` for comprehensive examples including:
- Basic command execution
- Variable expansion
- File operations
- Process management
- Error handling
- Command timing

## Testing

Run the test suite with:

```lisp
(load "test.lisp")
