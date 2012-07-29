
(in-package :cmd)

(defmacro with-open-streams (bindings &body body)
  (if bindings
      `(with-open-stream ,(car bindings)
         (with-open-streams ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defun %cmd (command input output wait)
  "Return a string, stream, or nil."
  ;; Create string streams if necessary
  (multiple-value-bind (io in out)
      ;; Okay, this is complicated.
      ;;
      ;; We are spcifying streams to be created for both input and
      ;; output, this is to give us a uniform return type and
      ;; predictable behvaior based on the other arguments.  If there
      ;; is no input, we just close the stream.
      ;;
      ;; We have to ask it to WAIT if there is input, although it
      ;; really doesn't.  When you ask for streams but tell it not to
      ;; wait, the input stream is ignored (WTF?).  We are slurping
      ;; output anyway, so this won't exit until the shell output
      ;; stream is closed (although the process could still be
      ;; running...)
      (ext:run-program "bash"
                       :arguments (list "-c" command)
                       :input input :output output
                       :wait (if (not input) wait t))
    (make-cmd-process
     :input in
     :output out
     :error nil
     :process-obj nil)))

