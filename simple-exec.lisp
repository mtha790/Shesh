;;;; Simple exec function example
;;;; Assumes *current-shell* is already initialized with a shell process

(defvar *current-shell* nil
        "The current shell process object from uiop:launch-program")

(defvar *stdout* ""
        "Buffer to store the shell output")

(defun stdout ()
  "Consume available output from the shell and store in *stdout*"
  (when *current-shell*
        (let ((output-stream (uiop:process-info-output *current-shell*)))
          (loop while (listen output-stream)
                do (let ((char (read-char-no-hang output-stream nil nil)))
                     (when char
                           (setf *stdout* (concatenate 'string *stdout* (string char)))))))))

(defun exec (command)
  "Execute a command in the current shell
   Examples: (exec \"pwd\") or (exec \"cd ..\")"
  (unless *current-shell*
    (error "No shell initialized in *current-shell*"))

  ;; Clear stdout buffer
  (setf *stdout* "")

  ;; Send the command to the shell
  (let ((input-stream (uiop:process-info-input *current-shell*)))
    (format input-stream "~A~%" command)
    (force-output input-stream))

  ;; Wait for command execution
  (sleep 0.2)

  ;; Consume the output
  (stdout)

  ;; Return the output
  *stdout*)

;;; Example initialization (commented out - assumes you'll do this elsewhere)
#|
(setf *current-shell* 
      (uiop:launch-program "bash"
                           :input :stream
                           :output :stream
                           :error-output :stream))
|#
