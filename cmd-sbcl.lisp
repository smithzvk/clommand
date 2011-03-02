(in-package :cmd)

(import '(sb-ext:process-output sb-ext:process-input))

(defun %pipe (commands input output)
  (cond ((null commands)
         (with-open-stream (echo (make-echo-stream input output))
           ;; We have an echo stream but we have to force the input to go
           ;; through.
           (iter (while (read-char echo nil nil)))
           (get-output-stream-string output) ))
        ((null (cdr commands))
         (%cmd (car commands) input output t)
         (get-output-stream-string output) )
        (t (with-open-stream (pipe
                              (sb-ext:process-output
                               (%cmd (car commands) input :stream nil )))
             (%pipe (cdr commands) pipe output) ))))

(defun %cmd (command input output wait)
  (let ((%input (cond ((typep input 'string)
                       (make-string-input-stream input) )
                      (t input) )))
    (let ((%output (cond ((eql output :string)
                          (make-string-output-stream) )
                         (t output) )))
      (let ((process (sb-ext:run-program "bash" (list "-c" command)
                                         :search t
                                         :if-input-does-not-exist :error
                                         :input %input
                                         :output %output
                                         :wait (if (or (eql output :stream)
                                                       (typep output 'stream) )
                                                   wait
                                                   t ))))
        (cond ((eql output :string) (get-output-stream-string
                                     (print %output) ))
              ((eql output :stream) (process-output process)) )))))
