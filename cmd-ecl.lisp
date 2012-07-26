
(in-package :cmd)

(defparameter *shell-spawn-time* .1
  "This is the time it takes for the implementation and system to spawn a shell.
This is a good heuristic for the polling interval.  This actually needs to be
measured at some point.")

;;<<>>=
(defstruct cmd-process
  input output error process-obj)

;;<<>>=
(defun cmd-process-status (process)
  (let ((status (if (cmd-process-p (cmd-process-process-obj process))
                    ;; Sometimes they are nested
                    (ext:external-process-status
                     (cmd-process-process-obj
                      (cmd-process-process-obj process)))
                    (ext:external-process-status
                     (cmd-process-process-obj process)))))
    (case status
      ((:stopped :running) status)
      (otherwise
       (cmd-process-exit-code process)))))
 
(defun cmd-process-exit-code (process)
  (ext:external-process-wait (cmd-process-process-obj process))
  (ext::external-process-%code (cmd-process-process-obj process)))

(defun cmd-process-term (process)
  (when (member (cmd-process-status process)
                '(:running :stopped))
    (%cmd (%mkstr "kill -SIGTERM " (ext:external-process-pid
                                    (cmd-process-process-obj process))))))

(defun %cmd (command input output error)
  (declare (ignore error))
  (multiple-value-bind (two-way exit-code process)
      (ext:run-program "bash" (list "-c" command)
                       :wait nil
                       :input input :output output
                       ;; ECL doesn't handle error output
                       :error nil)
    (declare (ignore two-way exit-code))
    (make-cmd-process :input (ext:external-process-input process)
                      :output (ext:external-process-output process)
                      :error nil
                      :process-obj process)))
