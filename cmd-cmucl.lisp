
(in-package :cmd)

;; (import '(extensions:process-output extensions:process-input))

;;<<>>=
(defstruct cmd-process
  input output error process-obj)

;;<<>>=
(defun cmd-process-status (process)
  (let ((status (if (cmd-process-p (cmd-process-process-obj process))
                    ;; Sometimes they are nested
                    (ext:process-status
                     (cmd-process-process-obj
                      (cmd-process-process-obj process)))
                    (ext:process-status
                     (cmd-process-process-obj process)))))
    (case status
      ((:stopped :running) status)
      (otherwise
       (ext:process-exit-code
        (cmd-process-process-obj process))))))

(defun cmd-process-exit-code (process)
  (ext:process-wait (cmd-process-process-obj process))
  (ext:process-exit-code (cmd-process-process-obj process)))

(defun cmd-process-term (process)
  (when (ext:process-pid (cmd-process-process-obj process))
    (ext:process-kill (cmd-process-process-obj process) unix:sigkill)))

;; Low level interface

;; (defun %pipe (commands input output)
;;   (cond ((null commands)
;;          (with-open-stream (echo (make-echo-stream input output))
;;            ;; We have an echo stream but we have to force the input to go
;;            ;; through.
;;            (iter (while (read-char echo nil nil)))
;;            (get-output-stream-string output)))
;;         ((null (cdr commands))
;;          (%cmd (car commands) input output t)
;;          (get-output-stream-string output))
;;         (t (with-open-stream (pipe
;;                               (sb-ext:process-output
;;                                (%cmd (car commands) input :stream nil)))
;;              (%pipe (cdr commands) pipe output)))))

(defparameter *shell-spawn-time* .1
  "This is the time it takes for the implementation and system to spawn a shell.
This is a good heuristic for the polling interval.  This actually needs to be
measured at some point.")

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
    (let* ((process (ext:run-program "bash" (list "-c" command)
                                     :if-input-does-not-exist :error
                                     :input %input
                                     :output %output
                                     :error %error
                                     :wait nil)))
      (make-cmd-process
       :input (ext:process-input process)
       :output (ext:process-output process)
       :error (ext:process-error process)
       :process-obj process))))
