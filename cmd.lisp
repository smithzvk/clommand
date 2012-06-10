
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

;; Some string utilities

(defun ~/ (&rest args)
  (concatenate
   'string
   "$HOME/" (apply #'./ (rest args))))

(defun /./ (&rest args)
  (concatenate
   'string
   "/" (apply #'./ (rest args))))

(defgeneric stringify (obj)
  (:method (obj)
    (with-output-to-string (out)
      (princ obj out))))

(defun ./ (&rest args)
  (if (null (rest args))
      (first args)
      (concatenate
       'string
       (first args)
       "/"
       (apply #'./ (rest args)))))

(defvar _ " ")

(defun %mkdstr (delim &rest args)
  "Make delimited string"
  (with-output-to-string (out)
    (iter (for a in args)
      (unless (first-iteration-p)
        (princ (stringify delim) out))
      (princ a out))))

(defun %mkstr (&rest args)
  "Make string"
  (with-output-to-string (out)
    (iter (for a in args)
      (princ a out))))


(defvar *shell-input* nil)

(defstruct cmd-process
  input output error process-obj)

(defun cmd-process-exit-code (process)
  (sb-impl::process-exit-code (cmd-process-process-obj process)))

(defmacro with-protected-binding ((binding-var binding-form binding-clean-up)
                                  &body body)
  (let ((binding-unbound (gensym)))
    `(let ((,binding-var ',binding-unbound))
       (unwind-protect
            (progn
              (setf ,binding-var ,binding-form)
              ,@body)
         (unless (eql ,binding-var ',binding-unbound)
           ,binding-clean-up)))))

(defun cmd-bg (command &key (input *shell-input*) (output :stream))
  "Run command in the background."
  (%cmd (%mkdstr
         " "
         "cd" (directory-namestring *default-pathname-defaults*)
         "&&" command)
        input output nil))

(defun cmd (command &key (input *shell-input*) (output :string)
                         (split-on nil) (trim-whitespace t)
                         error-on-exit-codes
                         error-unless-exit-codes
                         exit-code-hook)
  "Run command.

Input streams must be closed before output streams (in SBCL)."
  (when (eql input :stream)
    (error "You cannot use input as :stream here because this CMD blocks.  ~
            Use CMD-BG instead."))
  (when split-on
    (warn "Split-on is deprecated.  Use the pprce:split function itself."))
  (with-protected-binding
      (process (%cmd (%mkdstr
                      " "
                      "cd" (directory-namestring *default-pathname-defaults*)
                      "&&" command)
                     input output t)
               (sb-ext:process-kill (cmd-process-process-obj process)
                                    15))
    (when (and error-unless-exit-codes
               (not (member (cmd-process-exit-code process)
                            error-unless-exit-codes)))
      (cerror "Continue as if successful"
              "Command ~S exited with code ~A instead of one of ~A"
              command
              (cmd-process-exit-code process)
              error-unless-exit-codes))
    (when (member (cmd-process-exit-code process)
                  error-on-exit-codes)
      (cerror "Continue as if successful"
              "Command ~S exited with code ~A which is one of ~A"
              command
              (cmd-process-exit-code process)
              error-on-exit-codes))
    (when exit-code-hook
      (iter (for fn in (alexandria:ensure-list exit-code-hook))
        (funcall fn (cmd-process-exit-code process))))
    ;; Give simple output
    (cond
      ((eql output :string)
       (let ((output (cmd-process-output process)))
         (when trim-whitespace
           (setf output (string-trim '(#\Space #\Newline #\Tab) output)))
         (when split-on
           (setf output (ppcre:split split-on output)))
         output))
      ((eql output :stream)
       (cmd-process-output process))
      (t output))))

(defun cmd-p (command &key (true-vals '(0)) (false-vals '(1))
                           (input *shell-input*)
                           error-on-exit-codes
                           error-unless-exit-codes
                           exit-code-hook)
  "Run a shell command as a predicate"
  (let ((process (%cmd (%mkdstr
                        " "
                        "cd" (directory-namestring *default-pathname-defaults*)
                        "&&" command)
                       input :string t)))
    (when (and error-unless-exit-codes
               (not (member (cmd-process-exit-code process)
                            error-unless-exit-codes)))
      (cerror "Continue as if successful"
              "Command ~S exited with code ~A instead of one of ~A"
              command
              (cmd-process-exit-code process)
              error-unless-exit-codes))
    (when (member (cmd-process-exit-code process)
                  error-on-exit-codes)
      (cerror "Continue as if successful"
              "Command ~S exited with code ~A which is one of ~A"
              command
              (cmd-process-exit-code process)
              error-on-exit-codes))
    (when exit-code-hook
      (iter (for fn in (alexandria:ensure-list exit-code-hook))
        (funcall fn (cmd-process-exit-code process))))
    (let ((output (cmd-process-output process)))
      (cond ((member (cmd-process-exit-code process) true-vals)
             output)
            ((member (cmd-process-exit-code process) false-vals)
             nil)
            (t (cerror "Return NIL"
                       "~A exited with code ~A which is not a true value ~A ~
                        or a false value ~A."
                       command true-vals false-vals))))))


;; (defmacro with-cmd-options ((&key (wait t) input (output :string)) &body commands)
;;   (
