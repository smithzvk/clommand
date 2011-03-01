
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

(defvar *environment*)

(defvar *sync-environment*
  '(("PWD"
     (/. () "PWD=~A" *default-pathname-defaults*)
     (/. (line) (setf *default-pathname-defaults* (fad:pathname-as-directory
                                                  (pathname line) ))))))

#+sbcl(import '(sb-ext:process-output sb-ext:process-input))

;; In some imps we need to emulate the SBCL/CMUCL/ECL sort of process structure
#+clisp
(defstruct process
  output
  input )

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

#+sbcl
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

#+sbcl
(defun %cmd (command input output wait)
  (sb-ext:run-program "bash" (list "-c" command) :search t
                      :input input :output output
                      :wait wait ))

#+clisp
(defun %cmd (command input output wait)
  (cond ((typep output 'string-stream)
         (with-open-stream
             (echo (make-echo-stream
                    (ext:run-program "bash"
                                     :arguments (list "-c" command)
                                     :input input :output :stream
                                     :wait wait )
                    output ))
           ;; We have an echo stream but we have to force the input to go
           ;; through.
           (iter (while (read-char echo nil nil)))
           output ))
        ((typep output 'stream)
         (make-echo-stream 
          (ext:run-program "bash"
                           :arguments (list "-c" command)
                           :input input :output :stream
                           :wait wait )
          output ))
        ((eql output :stream)
         (ext:run-program "bash"
                          :arguments (list "-c" command)
                          :input input :output :stream
                          :wait wait ))
        (t (ext:run-program "bash"
                            :arguments (list "-c" command)
                            :input input :output output
                            :wait wait ))))

(defvar *trim-whitespace* t)

(defvar *split-on* nil)

(defvar *shell-input* nil)

(defun cmd (command &key (input *shell-input*) (output :string) (wait t))
  (when (eql output :string)
    (setf output (make-string-output-stream)) )
  (let ((process (%cmd (mkdstr
                        "cd" (directory-namestring *default-pathname-defaults*)
                        "&&"
                        command )
                       input output wait )))
    (cond ((not wait)
           process )
          ((typep output 'string-stream)
           (let ((output (get-output-stream-string output)))
             (when *trim-whitespace*
               (setf output (string-trim '(#\Space #\Newline #\Tab) output)) )
             (when *split-on*
               (setf output (ppcre:split *split-on* output)) )
             output ))
          ((eql output :stream)
           (process-output process) )
          (t output) )))

;; @\section{User Interface}

;; Eventhough it goes directly against rules I set for using reader macros, the
;; main interface for this library is via a reader macro.  I think this is okay
;; as it really is embedding another language inside CL, and so it really
;; deserves its own reader.

(defmethod translate-item (item)
  (format nil "~A" item) )

(defmethod translate-item ((item float))
  (format nil "~,,,,,,'eG" item) )

(defmethod translate-item ((item pathname))
  (format nil "~A" (namestring item)) )

(defun |#>-reader| (stream subchar arg)
  """
Read a `form' that will be sent to a shell for execution.  We will read in
characters and pass them directly to the shell except when we find a #\,.
Commas are treated as insertion points for Lisp expressions.  Put any form in
immediately after it and it's result will be placed there.  If you use ",@" the
result (which should be a list) will be spliced in using spaces as a delimiter.
Using ",d@" where `d' is any delimiting character, the result will be spliced in
delimited by that character.

This was used because it resembles quasiquote syntax and because commas are not
usually used in the shell.  If commas are needed, you can always escape them
with a `\' and the same is true of the few cases where `@' is used in an place
where it will confuse this reader.

Escaping characters are generally passed unchanged to the shell, so there is no
need to multiply escape character.  I.E. you don't need to specify escaped
quotes as \\\".  The one exception of this rule is on commas, since they are
really the only syntactic element here, the preceeding backslash will be removed
allowing you to pass unescaped commas to the shell.

Finally, if immediately after the closing parentheses, you place /form, the
output of the command will be sent through a PPCRE:SPLIT command.  Think of this
as fiddling with IFS.  Basically, the shell tokenizes input based on whatever is
in IFS.  This allows you to do the same.  Command/#\Newline splits into lines,
command/"\\s+" splits on any whitespace, command/"\\n\\s*\\n+" breaks whenever
one or more blank lines are detected.

Note: Piping in your shell code confuses SLIME's approximation of the Common
Lisp's reader.  I am considering including an alternate syntax that will involve
quotes hinting to slime that this isn't Lisp syntax it's reading.  Things work
if you use M-x slime-eval-region or M-x slime-eval-file and for some reason if
you evaluate only one toplevel form in the REPL, it works there as well.  I am
hoping that my forthcomming piping mechanism will remove this deficiency in an
amicable way.  As a work around, just pipe the output to "cat" in order to
balance vertical bars.
"""
  (declare (ignore subchar arg))
  (let* ((escaped nil)
         (quoted nil)
         (paren-level 0)
         (ext nil)
         (breaker nil)
         ;; A stupid parser, all it need to do is match parentheses when things
         ;; can be escaped.
         (command (iter (for c = (read-char stream t nil t))
                        (while c)
                        (cond
                          ;; Handle Lisp forms, this is really the only
                          ;; complicated bit
                          ((and (not escaped)
                                (eql c #\,) )
                           (let ((sym (gensym))
                                 (next (peek-char nil stream nil nil t)) )
                             (case next
                               (#\@ (read-char stream nil nil t)
                                  (push (list sym `(apply #'mkdstr
                                                          ,(read stream t nil t) ))
                                        ext ))
                               (otherwise
                                  (let ((possible-delim
                                         (read-char stream t nil t)))
                                    (cond ((eql #\@
                                                (peek-char nil stream nil nil t) )
                                           ;; This is a delimiter
                                           (read-char stream t nil t)
                                           (push (list sym `(apply #'mkdstr* ,possible-delim
                                                                   ,(read stream t nil t) ))
                                                 ext ))
                                          (t
                                           ;; Concatenate the possible-delim
                                           ;; character onto the beginning of
                                           ;; the input as it is not, in, fact,
                                           ;; a delimiter
                                           (let ((stream (make-concatenated-stream
                                                          (make-string-input-stream
                                                           (mkstr possible-delim) )
                                                          stream )))
                                             (push (list sym (read stream t nil t))
                                                   ext )))))))
                             (setf c sym) ))
                          ;; Handle shell commands
                          ((not (or quoted escaped))
                           (case c
                             (#\\ (setf escaped t))
                             (#\" (setf quoted #\"))
                             (#\' (setf quoted #\'))
                             (#\( (incf paren-level))
                             (#\) (decf paren-level)) ))
                          ;; End quoted state if necessary
                          ((eql quoted c)
                           (setf quoted nil) )
                          ;; End escaped state always on the next character
                          ;; (which is this one)
                          (escaped
                           (setf escaped nil) )
                          ;; This is here to catch the case where we have a
                          ;; backslash in a quoted environment.
                          ((eql #\\ c)
                           (setf escaped t) ))
                        (collect c)
                        (while (> paren-level 0))
                        (finally
                         (when (eql #\/ (peek-char nil stream nil nil t))
                           (read-char stream t nil t)
                           (push (read stream t nil t) breaker) )))))
    `(let ,(mapcar (/. (x) (list (car x)
                                `(translate-item
                                  (let ((*shell-input* nil)) ,(cadr x)) )))
            (reverse ext) )
       ,(if breaker
            `(ppcre:split ,(apply #'mkstr breaker)
                          (cmd (mkstr ,@command)) )
            `(cmd (mkstr ,@command)) ))))

(set-dispatch-macro-character #\# #\> '|#>-reader|)

;; (defmacro define-executable-functions (dir)
;;   (let ((*readtable* (copy-readtable *readtable*)))
;;     (setf (readtable-case *readtable*) :preserve)
;;     `(progn
;;        ,@(iter (for execucable in-stream (make-string-input-stream
;;                                           (cmd (mkstr "ls " dir)) ))
;;                (with-gensyms (args)
;;                  (collect `(defun ,execucable (&rest ,args)
;;                              (cmd (format nil "~a~{ ~a~}" ',execucable ,args)) )))))))

;; (defun |ls| (&rest args)
;;   (cmd (format nil "~a ~{~a ~}" '|ls| args)) )

;; (defun shell-test (node)
;;   (run-program "ssh" '("node1" "ls /") :search t :output t :input t :wait t) )
