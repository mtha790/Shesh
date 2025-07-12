;;;; Test file for the sh macro

;; Load the shesh system
(asdf:load-system "shesh")

(in-package :shesh)

;; Initialize shell
(init-shell)

(format t "~%=== Testing the sh macro ===~%~%")

;; Test 1: Simple commands without quotes
(format t "Test 1 - Using sh macro for pwd:~%")
(sh pwd)
(format t "Result: ~A~%" *stdout*)

;; Test 2: Commands with arguments
(format t "Test 2 - Using sh macro for ls with flags:~%")
(sh ls -la)
(format t "Result: ~A~%" *stdout*)

;; Test 3: Echo with multiple words
(format t "Test 3 - Using sh macro for echo:~%")
(sh echo Hello from Common Lisp)
(format t "Result: ~A~%" *stdout*)

;; Test 4: Change directory
(format t "Test 4 - Using sh macro for cd:~%")
(sh cd ..)
(sh pwd)
(format t "New directory: ~A~%" *stdout*)

;; Test 5: Comparison with exec function
(format t "Test 5 - Comparison:~%")
(format t "  Using exec: ~A~%" (exec "echo 'Hello World'"))
(format t "  Using sh: ")
(sh echo Hello World)
(format t "~A~%" *stdout*)

;; Clean up
(close-shell)
(format t "~%Tests completed!~%")
