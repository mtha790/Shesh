;;;; Demo of shesh functionality

(asdf:load-system "shesh")
(in-package :shesh)

(format t "~%~%===== SHESH DEMO =====~%~%")

;; Initialize shell
(format t "Initializing shell...~%")
(init-shell)

(format t "~%1. Basic exec function:~%")
(format t "   > (exec \"echo 'Hello from exec'\")~%")
(format t "   Result: ~A~%" (exec "echo 'Hello from exec'"))

(format t "~%2. Using sh macro without quotes:~%")
(format t "   > (sh echo Hello from sh macro)~%")
(sh echo Hello from sh macro)
(format t "   Result: ~A~%" *stdout*)

(format t "~%3. Commands with arguments:~%")
(format t "   > (sh ls -la | head -5)~%")
(exec "ls -la | head -5") ; Using exec for pipes
(format t "   Result:~%~A~%" *stdout*)

(format t "~%4. Working with variables:~%")
(format t "   > (sh export NAME=Shesh)~%")
(sh export NAME=Shesh)
(format t "   > (sh echo My name is $NAME)~%")
(exec "echo My name is $NAME")
(format t "   Result: ~A~%" *stdout*)

(format t "~%5. Multiple commands in sequence:~%")
(format t "   Creating a temporary file...~%")
(sh touch demo-temp.txt)
(exec "echo 'This is a test' > demo-temp.txt")
(format t "   > (sh cat demo-temp.txt)~%")
(sh cat demo-temp.txt)
(format t "   Result: ~A~%" *stdout*)
(sh rm demo-temp.txt)
(format t "   File cleaned up.~%")

(format t "~%6. Getting system information:~%")
(format t "   > (sh uname -a)~%")
(sh uname -a)
(format t "   Result: ~A~%" *stdout*)

(format t "~%7. Using exec for complex commands:~%")
(format t "   > (exec \"echo -e 'Line 1\\nLine 2\\nLine 3' | grep Line | wc -l\")~%")
(format t "   Result: ~A~%" (exec "echo -e 'Line 1\\nLine 2\\nLine 3' | grep Line | wc -l"))

;; Close shell
(close-shell)
(format t "~%Shell closed. Demo complete!~%~%")
