;;;; Run all tests for shesh

(format t "~%Loading shesh system...~%")
(asdf:load-system "shesh")

(in-package :shesh)

(defun test-exec-function ()
  "Test the basic exec function"
  (format t "~%=== Testing exec function ===~%")
  (init-shell)

  ;; Test 1: Simple command
  (let ((result (exec "echo 'Testing exec function'")))
    (format t "Test 1 - Echo: ~A" result))

  ;; Test 2: pwd command
  (let ((result (exec "pwd")))
    (format t "Test 2 - PWD: ~A" result))

  ;; Test 3: Command with pipes
  (let ((result (exec "echo 'Line 1\nLine 2\nLine 3' | wc -l")))
    (format t "Test 3 - Pipe: ~A" result))

  (close-shell)
  t)

(defun test-sh-macro ()
  "Test the sh macro"
  (format t "~%=== Testing sh macro ===~%")
  (init-shell)

  ;; Test 1: Simple command
  (sh echo Testing sh macro)
  (format t "Test 1 - Echo: ~A" *stdout*)

  ;; Test 2: Command with flags
  (sh ls -l)
  (format t "Test 2 - ls -l: ~A" (if (> (length *stdout*) 50)
                                     (format nil "~A..." (subseq *stdout* 0 50))
                                     *stdout*))

  ;; Test 3: Multiple word echo
  (sh echo This is a test with multiple words)
  (format t "Test 3 - Multi-word echo: ~A" *stdout*)

  ;; Test 4: Command with special characters (note: these become symbols)
  (sh echo test-with-dashes)
  (format t "Test 4 - Dashes: ~A" *stdout*)

  (close-shell)
  t)

(defun test-state-persistence ()
  "Test that shell state persists between commands"
  (format t "~%=== Testing state persistence ===~%")
  (init-shell)

  ;; Create a variable
  (exec "export MYVAR='Hello from Lisp'")

  ;; Use it in next command
  (exec "echo $MYVAR")
  (format t "Variable persistence: ~A" *stdout*)

  ;; Test cd persistence
  (sh pwd)
  (let ((original-dir *stdout*))
    (exec "cd ..")
    (sh pwd)
    (format t "Directory change: ~A -> ~A"
      (string-trim '(#\Newline #\Space) original-dir)
      (string-trim '(#\Newline #\Space) *stdout*)))

  (close-shell)
  t)

(defun run-all-tests ()
  "Run all tests"
  (format t "~%~%==== SHESH TEST SUITE ====~%")

  (handler-case
      (progn
       (test-exec-function)
       (test-sh-macro)
       (test-state-persistence)
       (format t "~%~%All tests completed successfully!~%"))
    (error (e)
      (format t "~%~%ERROR during tests: ~A~%" e))))

;; Run the tests
(run-all-tests)
