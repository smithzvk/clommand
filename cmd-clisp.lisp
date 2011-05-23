
(in-package :cmd)

(defmacro with-open-streams (bindings &body body)
  (if bindings
      `(with-open-stream ,(car bindings)
         (with-open-streams ,(cdr bindings)
           ,@body ))
      `(progn ,@body) ))

(defun %cmd (command input output wait)
  "Return a string, stream, or nil."
  ;; Create string streams if necessary
  (with-open-stream (%input (cond ((not input)
                                   (make-string-input-stream "") )
                                  ((typep input 'string)
                                   (make-string-input-stream input) )
                                  ((typep input 'pathname)
                                   (open input :direction :input) )
                                  ((typep input 'stream)
                                   input )
                                  (t (error "~S is not a valid input" input)) ))
    (let ((%output (cond ((eql output :string)
                          (make-string-output-stream) )
                         ((typep output 'pathname)
                          (open output :direction :output) )
                         ((or (typep output 'stream) (eql output :stream)
                              (eql output nil) )
                          output )
                         (t (error "~S is not a valid output" output)) )))
      (unwind-protect
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
                                :input :stream :output :stream
                                :wait (if (not input) wait t) )
             (with-open-streams ((io io) (out out))
               (unwind-protect
                    (progn
                      ;; Slurp INPUT and send it to the shell OUT
                      (with-open-stream (echo (make-echo-stream %input out))
                        (iter (for c = (read-char echo nil nil))
                              (while c) ))
                      (force-output out)
                      (close out)

                      ;; This effectively forces a wait
                      (when (typep %output 'stream)
                        ;; Slurp shell IN and send it to OUTPUT
                        (with-open-stream (echo (make-echo-stream in %output))
                          (iter (for c = (read-char echo nil nil))
                                (while c) )))
                      (cond ((eql output :stream)
                             (make-cmd-process :output in) )
                            ((eql output :string)
                             (make-cmd-process
                              :output
                              (get-output-stream-string %output) ))))
                 (unless (eql :stream %output) (close in)) )))
        (when (typep %output 'stream)
          (close %output) )))))
