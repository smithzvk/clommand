
(in-package :clommand)

;; @There is a concern about having too many open streams at once.  In SBCL,
;; having the program create a stream for you takes up a new file descriptor for
;; each call.  These are never reclaimed until explicitly closed.  Having it
;; write to a string stream, however, doesn't open a new FD, and there is no
;; problem.

;; @CLISP, on the other hand, appears to do a good job closing unused FDs,
;; perhaps on garbage collection.  This means that it is very difficult to make
;; CLISP run out of FDs unless you are activiely using all of them.  This is a
;; good thing, actually, since CLISP lacks the ability to specify a stream that
;; you want output to go to, meaning that using an `in memory' string is
;; impossible unless you wait for completion and slurp all output.

;; @So the interface I propose is one that:

;; @\item By default waits for completion and returns strings, first value
;; stdout, second value stderr.

;; @\item You may ask it to return a stream to you instead, or ask for a process 

;; @An important implementation note is that we do not allow for programs that
;; might cause deadlock.  By this I mean you cannot open a shell with both input
;; and output streams and then make the input dependent on the output.  While
;; doing so is defined well enough, it is a mine field for deadlock when it
;; comes to buffered I/O.

;; (defvar *environment*)

;; (defvar *sync-environment*
;;   '(("PWD"
;;      (/. () "PWD=~A" *default-pathname-defaults*)
;;      (/. (line) (setf *default-pathname-defaults* (fad:pathname-as-directory
;;                                                   (pathname line)))))))

;; Shell stuff

;; Make somethine almost as convenient as bash shell scripting but
;; easily accessed from Common Lisp

;; (defmacro in-bg (command)
;;   (let ((*wait* nil))
;;     (

;; (defmacro redirect-stream (from to)
;;   (

;; (defmacro redirect-streams (streams)

;; (defmacro piping (command &rest more-commands)
;;   (with-open-stream 
;;       (%cmd command :output 

;; These variables should not be part of the user interface.  It breaks
;; referential transparency for binding these to grossly change the behavior of
;; the library, but that is what it does.  These need to be set on a per
;; function call basis, so these should be keyword options to cmd and have a
;; special syntax for the reader.

(defvar *shell-input* nil)

(defvar *jobs* nil
  "Holds the process structures of all background jobs.")

(defparameter *jobs-lock* (bt:make-lock "bg-jobs"))

;; (when (streamp (cmd-process-input process))
;;   (close (cmd-process-input process)))
;; (when (streamp (cmd-process-output process))
;;   (close (cmd-process-output process)))
;; (when (streamp (cmd-process-error process))
;;   (close (cmd-process-error process)))

(defun remove-completed-jobs ()
  (setf *jobs*
        (remove-if-not
         (lambda (x) (member (cmd-process-status x) '(:running :stopped)))
         *jobs*)))

(defun all-jobs ()
  (bt:with-lock-held (*jobs-lock*)
    (remove-completed-jobs)
    (copy-list *jobs*)))

(defun handle-error-output (on-error-output
                            line
                            error-collector
                            error-cache)
  (case on-error-output
    (:warn (warn "~A" line))
    (:error (cerror "Continue" "~A" line))
    (otherwise
     (cond ((listp on-error-output)
            (mapcar (lambda (x) (funcall x line)) on-error-output))
           (t (error
               "I don't know how to deal with ~A for option on-error-output"
               on-error-output)))))
  (format error-cache "~A~%" line)
  (format error-collector "~A~%" line))

(defun handle-exit-code (command error-cache exit-code
                         error-unless-exit-codes error-on-exit-codes exit-code-hook)
  (cond ((and error-unless-exit-codes
              (not (member exit-code
                           error-unless-exit-codes)))
         (cerror "Continue as if successful"
                 "Command ~S exited with code ~A instead of one of ~A:~%~%  ~A"
                 command
                 exit-code
                 error-unless-exit-codes
                 error-cache))
        ((member exit-code
                 error-on-exit-codes)
         (cerror "Continue as if successful"
                 "Command ~S exited with code ~A which is one of ~A:~%~%  ~A"
                 command
                 exit-code
                 error-on-exit-codes
                 error-cache)))
  (when exit-code-hook
    (iter (for fn in (alexandria:ensure-list exit-code-hook))
      (funcall fn exit-code error-cache))))

(defun process-output-string (output trim-whitespace split-on)
  (let* ((output (if trim-whitespace
                     (string-right-trim '(#\Newline) output)
                     output)))
    (if split-on
        (ppcre:split split-on output)
        output)))

(defun cmd (command &key (input *shell-input*)
                         (output t)
                         (error t)
                         (on-error-output nil)
                         (split-on nil) (trim-whitespace t)
                         error-on-exit-codes
                         error-unless-exit-codes
                         exit-code-hook)
  "Run command."
  (let ((process
          (%cmd (%mkdstr
                 " "
                 "cd" (if (fad:directory-exists-p
                           (make-pathname :directory
                                          (pathname-directory
                                           *default-pathname-defaults*)))
                          (directory-namestring *default-pathname-defaults*)
                          "./")
                 "&&" (wrap-in-{} (if (consp command)
                                      (apply '%mkdstr " " command)
                                      command)))
                input :stream :stream)))
    (unwind-protect
         (let ((output-collector (make-string-output-stream))
               (error-collector (make-string-output-stream))
               (error-str (cmd-process-error process))
               (output-str (cmd-process-output process)))
           (bt:with-lock-held (*jobs-lock*)
             (remove-completed-jobs)
             (push process *jobs*))
           (let ((output-collector (if (eql output :error)
                                       error-collector
                                       output-collector))
                 (error-collector (if (eql error :output)
                                      output-collector
                                      error-collector))
                 (output-pipe (cl-plumbing:make-two-way-pipe))
                 (error-pipe (cl-plumbing:make-two-way-pipe))
                 (error-cache (make-string-output-stream)))
             (fair-process-output output-str error-str
                                  output-pipe error-pipe
                                  output-collector error-collector
                                  error-cache
                                  on-error-output)
             (handle-exit-code command
                               (get-output-stream-string error-cache)
                               (cmd-process-exit-code process)
                               error-unless-exit-codes
                               error-on-exit-codes exit-code-hook))
           (values
            (if output
                (process-output-string (get-output-stream-string output-collector)
                                       trim-whitespace split-on)
                "")
            (if error
                (process-output-string (get-output-stream-string error-collector)
                                       trim-whitespace split-on)
                "")
            (cmd-process-exit-code process)))
      (cmd-process-term process)
      (when (streamp (cmd-process-output process))
        (close (cmd-process-output process)))
      (when (streamp (cmd-process-error process))
        (close (cmd-process-error process))))))

(defun fair-process-output (output-str error-str
                            output-pipe error-pipe
                            output-collector error-collector
                            error-cache
                            on-error-output)
  (iter
    (for out-char = (if output-str
                        (read-char-no-hang output-str nil :eof nil)
                        :eof))
    (when out-char
      (format output-pipe "~A" out-char))
    (when (eql #\Newline out-char)
      (format output-collector
              "~A" (cl-plumbing:get-pipe-outlet-string output-pipe)))
    (for err-char = (if error-str
                        (read-char-no-hang error-str nil :eof nil)
                        :eof))
    (when err-char
      (format error-pipe "~A" err-char))
    (when (eql #\Newline err-char)
      (let ((line (cl-plumbing:get-pipe-outlet-string error-pipe)))
        (handle-error-output on-error-output line error-collector
                             error-cache)))
    (until (and (eql :eof out-char) (eql :eof err-char)))
    (when (and (member out-char '(:eof nil))
               (member err-char '(:eof nil)))
      (sleep *shell-spawn-time*))))

(defun cmd-bg (command &key (input *shell-input*)
                            (output t)
                            (error t)
                            (on-error-output nil)
                            error-on-exit-codes
                            error-unless-exit-codes
                            exit-code-hook)
  "Run command."
  (let ((process
          (%cmd (%mkdstr
                 " "
                 "cd" (if (fad:directory-exists-p
                           (make-pathname :directory
                                          (pathname-directory
                                           *default-pathname-defaults*)))
                          (directory-namestring *default-pathname-defaults*)
                          "./")
                 "&&" (wrap-in-{} (if (consp command)
                                      (apply '%mkdstr " " command)
                                      command)))
                input :stream :stream)))
    (let ((output-collector (cl-plumbing:make-two-way-pipe))
          (error-collector (cl-plumbing:make-two-way-pipe))
          (error-str (cmd-process-error process))
          (output-str (cmd-process-output process)))
      (bt:with-lock-held (*jobs-lock*)
        (remove-completed-jobs)
        (push process *jobs*))
      (let ((thread
              (bt:make-thread
               (lambda ()
                 (unwind-protect
                      (let ((output-collector (if (eql output :error)
                                                  error-collector
                                                  output-collector))
                            (error-collector (if (eql error :output)
                                                 output-collector
                                                 error-collector))
                            (error-cache (make-string-output-stream))
                            (output-pipe (cl-plumbing:make-two-way-pipe))
                            (error-pipe (cl-plumbing:make-two-way-pipe)))
                        (fair-process-output output-str error-str
                                             output-pipe error-pipe
                                             output-collector error-collector
                                             error-cache
                                             on-error-output)
                        (handle-exit-code command
                               (get-output-stream-string error-cache)
                               (cmd-process-exit-code process)
                               error-unless-exit-codes
                               error-on-exit-codes exit-code-hook))
                   (cmd-process-term process)
                   (when (streamp (cmd-process-output process))
                     (close (cmd-process-output process)))
                   (when (streamp (cmd-process-error process))
                     (close (cmd-process-error process))))))))
        (values
         (make-cmd-process
          :input input
          :output output-collector
          :error error-collector
          :process-obj process)
         thread)))))

(defun cmd-p (command &key (true-vals '(0)) (false-vals '(1))
                           (input *shell-input*)
                           on-error-output
                           error-on-exit-codes
                           error-unless-exit-codes
                           exit-code-hook)
  "Run a shell command as a predicate"
  (multiple-value-bind (output error-output exit-code)
          (cmd command :input input
                       :output t
                       :error t
                       :on-error-output on-error-output
                       :error-on-exit-codes error-on-exit-codes
                       :error-unless-exit-codes error-unless-exit-codes
                       :exit-code-hook exit-code-hook)
    (cond ((member exit-code true-vals) output)
          ((member exit-code false-vals) nil)
          (t (cerror "Return NIL"
                     "~A exited with code ~A which is not a true ~@
                      value ~A or a false value ~A:~%~%  ~A"
                     command exit-code
                     true-vals false-vals
                     error-output)))))

;; If wait is specified non-nil, then it will not exit until the command has
;; completed.

;; If wait is specified nil or not specified, then command will still wait
;; unless the output is specified to be a stream or nil and the input is
;; specified to be a stream, a string, or nil and the error output is either
;; :output or nil.

;; The return value is the output.  That output is either string, a stream, or
;; or a list of two of these objects (standard and error output) if you
;; requested them separate, or nil.  The second return value is the 

(defun scan-for-warn (line)
  (when (ppcre:scan (ppcre:create-scanner "warn" :case-insensitive-mode t) line)
    (warn line)))

(defun scan-for-error (line)
  (when (ppcre:scan (ppcre:create-scanner "error" :case-insensitive-mode t) line)
    (warn line)))

;; (defvar *stderr-hook* '(scan-for-warn scan-for-error))

;; (defvar *stdout-hook* nil)

;; It is often necessary to wrap more complicated commands in Lisp functions of
;; their own.

;; (defmacro with-cmd-options ((&key (wait t) input (output :string)) &body commands)
;;   (
