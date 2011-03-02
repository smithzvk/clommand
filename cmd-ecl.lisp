
(in-package :cmd)


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
           (let ((io (ext:run-program "bash" 
                                      :arguments (list "-c" command)
                                      :input :stream :output :stream
                                      :wait (if (not input) wait t) )))
             ;; Slurp INPUT and send it to the shell OUT
             (with-open-stream (echo (make-echo-stream
                                      %input
                                      (two-way-stream-output-stream io) ))
               (iter (for c in-stream echo using #'read-char)))
             (force-output (two-way-stream-output-stream io))

             ;; This effectively forces a wait
             (when (typep %output 'stream)
               ;; Slurp shell IN and send it to OUTPUT
               (with-open-stream (echo (make-echo-stream in %output))
                 (iter (for c = (read-char echo nil nil))
                       (while c) )))
             (cond ((eql output :stream) in)
                            ((eql output :string)
                             (get-output-stream-string %output) )))
                 (unless (eql :stream %output) (close in)) )))
        (when (typep %output 'stream)
          (close %output) ))
