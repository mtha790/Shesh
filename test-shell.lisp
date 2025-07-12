;;;; Test file for the shesh interface

;; Load the shesh interface
(load "shesh.lisp")

(in-package :shesh)

;; Simple test
(defun test-basic-commands ()
  "Test basic shell commands"
  (format t "~%Starting basic shell test...~%~%")

  ;; Initialize the shell
  (init-shell)

  ;; Test 1: pwd
  (format t "Test 1 - Current directory:~%")
  (format t "Command: pwd~%")
  (format t "Result: ~A~%" (exec "pwd"))

  ;; Test 2: Simple echo
  (format t "~%Test 2 - Echo command:~%")
  (format t "Command: echo 'Hello from Common Lisp!'~%")
  (format t "Result: ~A~%" (exec "echo 'Hello from Common Lisp!'"))

  ;; Test 3: List files
  (format t "~%Test 3 - List files:~%")
  (format t "Command: ls~%")
  (format t "Result: ~A~%" (exec "ls"))

  ;; Test 4: Change directory and verify
  (format t "~%Test 4 - Change directory:~%")
  (format t "Command: cd ..~%")
  (exec "cd ..")
  (format t "Command: pwd~%")
  (format t "Result: ~A~%" (exec "pwd"))

  ;; Test 5: Multiple commands
  (format t "~%Test 5 - Create and remove a file:~%")
  (format t "Command: touch test-file.txt~%")
  (exec "touch test-file.txt")
  (format t "Command: ls test-file.txt~%")
  (format t "Result: ~A~%" (exec "ls test-file.txt"))
  (format t "Command: rm test-file.txt~%")
  (exec "rm test-file.txt")

  ;; Clean up
  (close-shell)
  (format t "~%Test completed.~%"))

;; Run the test
(test-basic-commands)
