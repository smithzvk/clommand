(in-package :cmd)

;; (import '(sb-ext:process-output sb-ext:process-input))

(defun %pipe (commands input output)
  (cond ((null commands)
         (with-open-stream (echo (make-echo-stream input output))
           ;; We have an echo stream but we have to force the input to go
           ;; through.
           (iter (while (read-char echo nil nil)))
           (get-output-stream-string output)))
        ((null (cdr commands))
         (%cmd (car commands) input output t)
         (get-output-stream-string output))
        (t (with-open-stream (pipe
                              (sb-ext:process-output
                               (%cmd (car commands) input :stream nil)))
             (%pipe (cdr commands) pipe output)))))

(defparameter *shell-spawn-time* .1
  "This is the time it takes for the implementation and system to spawn a shell.
This is a good heuristic for the polling interval.  This actually needs to be
measured at some point.")

(defun %cmd (command input output wait)
  (let ((%input (cond ((typep input 'string)
                       (make-string-input-stream input))
                      (t input)))
        (%output (cond ((eql output :string) :stream)
                       (t output))))
    ;; SBCL doesn't handle specifying string-streams for output when you are not
    ;; going to wait.  This seems like a bug, but for now we limp along.
    (let* ((process (sb-ext:run-program "bash" (list "-c" command)
                                        :search t
                                        :if-input-does-not-exist :error
                                        :input %input
                                        :output %output
                                        :error :stream
                                        :wait nil)))
      (handler-case
          (progn
            (when wait (iter (while (sb-ext:process-alive-p process))
                         (sleep *shell-spawn-time*)))
            (make-cmd-process
             :input (cond ((eql input :stream)
                           (sb-ext:process-input process))
                          (t %input))
             :output (cond ((eql output :string)
                            (with-output-to-string (out)
                              (iter
                               (for line in-stream
                                 (sb-ext:process-output process)
                                 using #'read-line)
                               (format out "~A~%" line))))
                           (t (sb-ext:process-output process)))
             :error (with-output-to-string (out)
                      (iter
                        (for line in-stream
                          (sb-ext:process-error process)
                          using #'read-line)
                        (format out "~A~%" line)))
             :process-obj process))
        (sb-sys:interactive-interrupt (condition)
          (sb-ext:process-kill process sb-posix:sigstop)
          (prog1 (cerror "Continue" condition)
            (sb-ext:process-kill process sb-posix:sigcont)))))))
