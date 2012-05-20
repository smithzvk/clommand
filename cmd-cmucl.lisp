
(in-package :cmd)

(import '(extensions:process-output extensions:process-input))

(defun %cmd (command input output wait)
  (let ((%input (cond ((typep input 'string)
                       (make-string-input-stream input))
                      (t input))))
    (let ((%output (cond ((eql output :string)
                          (make-string-output-stream))
                         (t output))))
      (let ((process (extensions:run-program
                      "bash" (list "-c" command)
                      :if-input-does-not-exist :error
                      :input %input
                      :output %output
                      :wait (if (or (eql output :stream)
                                    (typep output 'stream))
                                wait
                                t))))
        (cond ((eql output :string) (get-output-stream-string
                                     (print %output)))
              ((eql output :stream) (process-output process)))))))

