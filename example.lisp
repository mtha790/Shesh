;;;; Shesh Examples - Demonstrating shell interaction from Common Lisp

;; Load the shesh system
(asdf:load-system "shesh")

(in-package :shesh)

(defun example-basic-usage ()
  "Demonstrate basic shell command execution"
  (format t "~%=== Basic Usage Examples ===~%~%")

  ;; Initialize the shell
  (init-shell)

  ;; Example 1: Simple command
  (format t "1. Getting current directory:~%")
  (sh pwd)
  (format t "   Result: ~A~%~%" *stdout*)

  ;; Example 2: Command with arguments
  (format t "2. Listing files:~%")
  (sh ls -la)
  (format t "   Files in current directory...~%~%")

  ;; Example 3: Using echo without quotes
  (format t "3. Echo without quotes:~%")
  (sh echo Hello World from Common Lisp)
  (format t "   Result: ~A~%~%" *stdout*)

  ;; Cleanup
  (close-shell))

(defun example-variable-expansion ()
  "Demonstrate variable expansion with $ syntax"
  (format t "~%=== Variable Expansion Examples ===~%~%")

  (init-shell)

  ;; Example 1: Simple variable
  (format t "1. Simple variable substitution:~%")
  (let ((name "Alice"))
    (sh echo Hello $name)
    (format t "   Result: ~A~%~%" *stdout*))

  ;; Example 2: Numeric calculations
  (format t "2. Numeric calculation:~%")
  (let ((x 10)
        (y 5))
    (let ((result (* x y)))
      (sh echo $x times $y equals $result)
      (format t "   Result: ~A~%~%" *stdout*)))

  ;; Example 3: Variable with spaces
  (format t "3. Variable containing spaces:~%")
  (let ((message "Common Lisp rocks!"))
    (sh echo Message is: $message)
    (format t "   Result: ~A~%~%" *stdout*))

  (close-shell))

(defun example-practical-use ()
  "Practical examples of shell integration"
  (format t "~%=== Practical Examples ===~%~%")

  (init-shell)

  ;; Example 1: Creating and using a temporary file
  (format t "1. Working with files:~%")
  (let ((filename "/tmp/shesh-test.txt")
        (content "Hello from Shesh!"))
    (exec (format nil "echo '~A' > ~A" content filename))
    (sh cat $filename)
    (format t "   File contents: ~A~%" *stdout*)
    (sh rm $filename)
    (format t "   File cleaned up.~%~%"))

  ;; Example 2: Process information
  (format t "2. Getting process information:~%")
  (exec "ps aux | grep sbcl | head -1")
  (format t "   Current SBCL process: ~A~%~%"
    (subseq *stdout* 0 (min 60 (length *stdout*))))

  ;; Example 3: Git status (if in a git repo)
  (format t "3. Checking git status:~%")
  (exec "git status --short 2>/dev/null || echo 'Not a git repository'")
  (format t "   Status: ~A~%~%" *stdout*)

  (close-shell))

(defun example-advanced-features ()
  "Demonstrate advanced features"
  (format t "~%=== Advanced Features ===~%~%")

  (init-shell)

  ;; Example 1: Command timing
  (format t "1. Command completion detection:~%")
  (let ((start (get-internal-real-time)))
    (exec "sleep 0.3 && echo 'Task completed'")
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)))
      (format t "   Result: ~A~%" *stdout*)
      (format t "   Time taken: ~,3F seconds~%~%" elapsed)))

  ;; Example 2: Shell state preservation
  (format t "2. Shell state is preserved between commands:~%")
  (sh cd /tmp)
  (sh pwd)
  (format t "   Current dir: ~A~%" *stdout*)
  (sh cd -)
  (sh pwd)
  (format t "   Back to: ~A~%~%" *stdout*)

  ;; Example 3: Error handling
  (format t "3. Error handling:~%")
  (exec "ls /nonexistent 2>&1 || echo 'Directory not found'")
  (format t "   Result: ~A~%~%" *stdout*)

  (close-shell))

(defun run-all-examples ()
  "Run all example functions"
  (example-basic-usage)
  (example-variable-expansion)
  (example-practical-use)
  (example-advanced-features)
  (format t "~%=== All examples completed! ===~%"))

;; Run examples when loaded
(format t "~%Shesh examples loaded. Run (run-all-examples) to see all examples.~%")
(format t "Or run individual examples:~%")
(format t "  - (example-basic-usage)~%")
(format t "  - (example-variable-expansion)~%")
(format t "  - (example-practical-use)~%")
(format t "  - (example-advanced-features)~%~%")
