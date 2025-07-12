# Shesh - Shell Interface for Common Lisp

A simple Common Lisp interface for interacting with shell processes using UIOP.

## Files

- `shesh.lisp` - Main implementation with full features
- `simple-exec.lisp` - Minimal example of just the `exec` function
- `test-shell.lisp` - Test file with usage examples
- `shesh.asd` - ASDF system definition

## Usage

### Loading with ASDF

```lisp
(asdf:load-system "shesh")
```

### Basic Usage

```lisp
;; Load the package
(in-package :shesh)

;; Initialize a shell
(init-shell)

;; Execute commands with the exec function
(exec "pwd")           ; Get current directory
(exec "ls -la")        ; List files
(exec "cd ..")         ; Change directory
(exec "echo 'Hello'")  ; Echo text

;; Or use the sh macro to write commands without quotes
(sh pwd)               ; Get current directory
(sh ls -la)            ; List files
(sh cd ..)             ; Change directory
(sh echo Hello World)  ; Echo text without quotes

;; The output is stored in *stdout*
(print *stdout*)

;; Close the shell when done
(close-shell)
```
