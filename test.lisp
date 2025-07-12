;;;; Comprehensive test file for Shesh

;; Load the shesh system
(asdf:load-system "shesh")

(in-package :shesh)

;; Initialize shell
(init-shell)

(format t "~%=== SHESH COMPREHENSIVE TESTS ===~%~%")

;;; ===============================================
;;; SECTION 1: Basic functionality tests
;;; ===============================================
(format t "--- Section 1: Basic functionality ---~%~%")

;; Test 1.1: Simple commands
(format t "Test 1.1 - Simple command (pwd):~%")
(sh pwd)
(format t "Result: ~A~%~%" *stdout*)

;; Test 1.2: Commands with arguments
(format t "Test 1.2 - Command with arguments (ls -la | head -5):~%")
(exec "ls -la | head -5")
(format t "Result: ~A~%~%" *stdout*)

;; Test 1.3: Echo without quotes
(format t "Test 1.3 - Echo without quotes:~%")
(sh echo hello world from lisp)
(format t "Result: ~A~%~%" *stdout*)

;;; ===============================================
;;; SECTION 2: Variable expansion tests
;;; ===============================================
(format t "--- Section 2: Variable expansion with $ ---~%~%")

;; Test 2.1: Simple variable
(format t "Test 2.1 - Simple variable expansion:~%")
(let ((x 42))
  (sh echo The answer is $x))
(format t "Result: ~A~%~%" *stdout*)

;; Test 2.2: Multiple variables
(format t "Test 2.2 - Multiple variables:~%")
(let ((name "Alice")
      (age 30))
  (sh echo Hello $name you are $age years old))
(format t "Result: ~A~%~%" *stdout*)

;; Test 2.3: Variable with spaces
(format t "Test 2.3 - Variable containing spaces:~%")
(let ((message "Hello World from Lisp"))
  (sh echo Message is $message))
(format t "Result: ~A~%~%" *stdout*)

;; Test 2.4: Numeric calculation
(format t "Test 2.4 - Numeric calculation:~%")
(let ((a 5)
      (b 3))
  (let ((sum (+ a b)))
    (sh echo $a + $b = $sum)))
(format t "Result: ~A~%~%" *stdout*)

;; Test 2.5: Variable at beginning
(format t "Test 2.5 - Variable at beginning of command:~%")
(let ((cmd "echo"))
  (sh $cmd This is a test))
(format t "Result: ~A~%~%" *stdout*)

;;; ===============================================
;;; SECTION 3: Unbound variable handling
;;; ===============================================
(format t "--- Section 3: Unbound variable handling ---~%~%")

;; Test 3.1: Unbound variable (passed to shell)
(format t "Test 3.1 - Unbound variable (should be empty):~%")
(sh echo $UNDEFINED_VAR)
(format t "Result: '~A'~%~%" *stdout*)

;; Test 3.2: Mix of bound and unbound
(format t "Test 3.2 - Mix of bound and unbound:~%")
(let ((x 100))
  (sh echo x=$x but y=$y))
(format t "Result: ~A~%~%" *stdout*)

;; Test 3.3: Shell environment variable
(format t "Test 3.3 - Shell environment variable:~%")
(exec "echo HOME=$HOME")
(format t "Result: ~A~%~%" *stdout*)

;;; ===============================================
;;; SECTION 4: Command completion detection
;;; ===============================================
(format t "--- Section 4: Command completion detection ---~%~%")

;; Test 4.1: Fast command timing
(format t "Test 4.1 - Fast command timing:~%")
(let ((start-time (get-internal-real-time)))
  (sh echo Quick test)
  (let ((elapsed (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second)))
    (format t "Result: ~A~%" *stdout*)
    (format t "Time taken: ~,3F seconds~%~%" elapsed)))

;; Test 4.2: Slow command timing
(format t "Test 4.2 - Command with 0.5 second delay:~%")
(let ((start-time (get-internal-real-time)))
  (exec "sleep 0.5 && echo 'After sleep'")
  (let ((elapsed (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second)))
    (format t "Result: ~A~%" *stdout*)
    (format t "Time taken: ~,3F seconds (should be > 0.5)~%~%" elapsed)))

;; Test 4.3: Empty output command
(format t "Test 4.3 - Command with no output:~%")
(sh true)
(format t "Result (should be empty): '~A'~%~%" *stdout*)

;; Test 4.4: Error handling
(format t "Test 4.4 - Command with error:~%")
(exec "ls /nonexistent 2>&1 || echo 'Error handled'")
(format t "Result: ~A~%~%" *stdout*)

;;; ===============================================
;;; SECTION 5: Advanced usage
;;; ===============================================
(format t "--- Section 5: Advanced usage ---~%~%")

;; Test 5.1: Chained commands
(format t "Test 5.1 - Chained commands:~%")
(exec "echo 'First' && echo 'Second' && echo 'Third'")
(format t "Result: ~A~%~%" *stdout*)

;; Test 5.2: Variable in path
(format t "Test 5.2 - Variable in file path:~%")
(let ((dir "/tmp"))
  (exec (format nil "ls ~A | head -3" dir)))
(format t "Result (first 3 lines): ~A~%~%" *stdout*)

;; Test 5.3: Complex mixed usage
(format t "Test 5.3 - Complex example with variables:~%")
(let ((prefix "RESULT")
      (count 3))
  (sh echo $prefix - Found $count items in $UNKNOWN_DIR))
(format t "Result: ~A~%~%" *stdout*)

;; Test 5.4: State preservation
(format t "Test 5.4 - Shell state preservation:~%")
(sh cd /tmp)
(sh pwd)
(format t "Current directory: ~A~%" *stdout*)
(sh cd -)
(sh pwd)
(format t "Back to: ~A~%~%" *stdout*)

;;; ===============================================
;;; Cleanup
;;; ===============================================
(close-shell)
(format t "=== ALL TESTS COMPLETED ===~%")
