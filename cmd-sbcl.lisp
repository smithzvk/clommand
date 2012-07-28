(in-package :cmd)

;; (import '(sb-ext:process-output sb-ext:process-input))

;;<<>>=
(defstruct cmd-process
  input output error process-obj)

(defun raw-process-obj (process)
  (if (sb-ext:process-p process)
      process
      (raw-process-obj (cmd-process-process-obj process))))

;;<<>>=
(defun cmd-process-status (process)
  (let* ((raw-process (raw-process-obj process))
         (status (sb-ext:process-status raw-process)))
    (case status
      ((:stopped :running) status)
      (otherwise
       (sb-ext:process-exit-code raw-process)))))

(defun cmd-process-exit-code (process)
  ;; ;; This is an alternate implementation that sometimes works even if your
  ;; ;; are using signals.
  ;; (nth-value 1 (sb-posix:waitpid
  ;;               (sb-ext:process-pid
  ;;                (cmd-process-process-obj process)) 0))

  ;; This should work, but it doesn't if you are sending signals to your
  ;; processes
  (sb-ext:process-wait (raw-process-obj process))
  (sb-ext:process-exit-code (raw-process-obj process)))

(defun cmd-process-term (process)
  (when (sb-ext:process-alive-p (raw-process-obj process))
    (sb-ext:process-kill (raw-process-obj process) sb-posix:sigkill)))

;; Low level interface

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

;; @The function <<%cmd>> creates a background process and returns a cmd-process
;; structure.  This structure must have the requested stream slots filled in.

(defun %cmd (command input output error)
  (let ((%input (cond ((typep input 'string)
                       (make-string-input-stream input))
                      (t input)))
        (%output (cond ((eql output :string) :stream)
                       (t output)))
        (%error (cond ((eql error :string) :stream)
                      ((eql error :error) :stream)
                      ((eql error :warn) :stream)
                      (t error))))
    ;; SBCL doesn't handle specifying string-streams for output when you are not
    ;; going to wait.  This seems like a bug, but for now we limp along.
    (let* ((process (sb-ext:run-program "bash" (list "-c" command)
                                        :search t
                                        :if-input-does-not-exist :error
                                        :input %input
                                        :output %output
                                        :error %error
                                        :wait nil)))
      (make-cmd-process
       :input (sb-ext:process-input process)
       :output (sb-ext:process-output process)
       :error (sb-ext:process-error process)
       :process-obj process))))



;; (sb-sys:interactive-interrupt (condition)
;;           (sb-ext:process-kill process sb-posix:sigstop)
;;           (prog1 (cerror "Continue" condition)
;;             (sb-ext:process-kill process sb-posix:sigcont)))

;; (defun %cmd2 (command input output error-in-output wait)
;;   (when (and (not wait) (typep output 'string-stream))
;;     (warn "SBCL doesn't handle specifying string-streams for output when you are not going to wait."))
;;   (let ((%input (cond ((typep input 'string)
;;                        (make-string-input-stream input))
;;                       (t input)))
;;         (%output (cond ((eql output :string)
;;                         (make-string-output-stream))
;;                        (t output))))
;;     (let ((process (sb-ext:run-program "bash" (list "-c" command)
;;                                        :search t
;;                                        :if-input-does-not-exist :error
;;                                        :input %input
;;                                        :output %output
;;                                        :wait (if (eql output :string)
;;                                                  t
;;                                                  wait))))
;;       (make-cmd-process
;;        :input (cond ((eql input :stream)
;;                      (sb-ext:process-input process))
;;                     (t %input))
;;        :output (cond ((eql output :string)
;;                       (get-output-stream-string %output))
;;                      (t (sb-ext:process-output process)))))))
