;;;; Shesh - Shell Interface for Common Lisp
;;;; Using UIOP to interact with a shell process

(defpackage :shesh
  (:use :cl)
  (:export #:*current-shell*
           #:*stdout*
           #:init-shell
           #:exec
           #:stdout
           #:close-shell
           #:sh))

(in-package :shesh)

;;; Special variables
(defvar *current-shell* nil
        "The current shell process object")

(defvar *stdout* ""
        "Buffer to store the shell output")

(defvar *shell-output-stream* nil
        "The output stream of the shell process")

(defvar *shell-input-stream* nil
        "The input stream of the shell process")

;;; Initialize the shell
(defun init-shell (&optional (shell-command "bash"))
  "Initialize a new shell process"
  (when *current-shell*
        (close-shell))

  ;; Launch the shell with interactive streams
  (setf *current-shell*
    (uiop:launch-program shell-command
      :input :stream
      :output :stream
      :error-output :stream))

  ;; Store the streams for easy access
  (setf *shell-input-stream* (uiop:process-info-input *current-shell*)
    *shell-output-stream* (uiop:process-info-output *current-shell*))

  ;; Clear stdout buffer
  (setf *stdout* "")

  ;; Set non-interactive mode for more predictable output
  (format *shell-input-stream* "set +o vi~%") ; Disable vi mode if set
  (format *shell-input-stream* "set +o emacs~%") ; Disable emacs mode if set
  (format *shell-input-stream* "PS1='$ '~%") ; Simple prompt
  (force-output *shell-input-stream*)

  ;; Consume initial output
  (sleep 0.1)
  (stdout)

  *current-shell*)

;;; Function to consume stdout
(defun stdout ()
  "Consume available output from the shell and append to *stdout*"
  (when (and *current-shell* *shell-output-stream*)
        (loop while (listen *shell-output-stream*)
              do (let ((char (read-char-no-hang *shell-output-stream* nil nil)))
                   (when char
                         (setf *stdout* (concatenate 'string *stdout* (string char))))))))

;;; Main exec function
(defun exec (command)
  "Execute a command in the current shell"
  (unless *current-shell*
    (error "No shell initialized. Call (init-shell) first."))

  ;; Clear stdout buffer before executing
  (setf *stdout* "")

  ;; Send the command to the shell
  (format *shell-input-stream* "~A~%" command)
  (force-output *shell-input-stream*)

  ;; Wait a bit for the command to execute
  ;; This might need adjustment based on command complexity
  (sleep 0.2)

  ;; Consume the output
  (stdout)

  ;; Return the output
  *stdout*)

;;; Macro to write commands without quotes
(defmacro sh (&rest command-parts)
  "Execute a shell command without needing quotes.
   Examples:
   (sh pwd)
   (sh ls -la)
   (sh echo hello world)
   (sh cd ..)
   
   The command parts are converted to strings and joined with spaces."
  `(exec (format nil "~{~A~^ ~}"
           (list ,@(loop for part in command-parts
                         collect `(princ-to-string ',part))))))

;;; Close the shell
(defun close-shell ()
  "Close the current shell process"
  (when *current-shell*
        (ignore-errors
          (format *shell-input-stream* "exit~%")
          (force-output *shell-input-stream*))
        (uiop:terminate-process *current-shell*)
        (setf *current-shell* nil
          *shell-input-stream* nil
          *shell-output-stream* nil
          *stdout* "")))

;;; Example usage functions
(defun example-usage ()
  "Show example usage of the shell interface"
  (format t "~%=== Shell Interface Example ===~%~%")

  ;; Initialize shell
  (format t "Initializing shell...~%")
  (init-shell)

  ;; Execute pwd
  (format t "~%Executing 'pwd':~%")
  (let ((result (exec "pwd")))
    (format t "Output: ~A" result))

  ;; Execute ls
  (format t "~%Executing 'ls -la':~%")
  (let ((result (exec "ls -la")))
    (format t "Output: ~A" result))

  ;; Execute cd and then pwd to verify
  (format t "~%Executing 'cd ..' and then 'pwd':~%")
  (exec "cd ..")
  (let ((result (exec "pwd")))
    (format t "Output after cd: ~A" result))

  ;; Clean up
  (format t "~%Closing shell...~%")
  (close-shell)
  (format t "Done.~%"))

;;; Alternative exec with timeout
(defun exec-with-timeout (command &optional (timeout 1.0))
  "Execute a command with a timeout for output collection"
  (unless *current-shell*
    (error "No shell initialized. Call (init-shell) first."))

  ;; Clear stdout buffer
  (setf *stdout* "")

  ;; Send command
  (format *shell-input-stream* "~A~%" command)
  (force-output *shell-input-stream*)

  ;; Collect output with timeout
  (let ((start-time (get-internal-real-time)))
    (loop while (< (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)
                   timeout)
          do (stdout)
            (sleep 0.05)))

  *stdout*)
