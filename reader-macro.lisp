
(in-package :cmd)

;; @\section{User Interface}

;;<<>>=
(defvar *remove-newlines* t
  "Removing the newlines from the read command allows the syntax to integrate
more readily with CL's, which ignores newlines.  This means that your shell code
needs to have its lines terminated with a semicolon, `;', or other command
separator.  All in all, a pretty small price to pay for the better integration.

This only applies to the reader macro interface; not to CMD.

However, if you don't desire this behavior, you may bind it to NIL for the
duration of the `#>' command form \(newline removal happens are runtime, not
readtime).  Be aware that if you do so, you are responsible for escaping
unwanted newlines when there are line breaks in your shell code, but not the
Lisp code \(including comma evaluated forms).  For instance:

 #>(shell-command \\
       ,(some-lisp 
         blah
         blech) \\
     more shell stuff
     another-command)

With *remove-newlines* non-nil, the command above could look like:

 #>(shell-command
       ,(some-lisp
         blah
         blech)
     more shell stuff;
     another-command)
")

;; Eventhough it goes directly against rules I set for using reader macros, the
;; main interface for this library is via a reader macro.  I think this is okay
;; as it really is embedding another language inside CL, and so it really
;; deserves its own reader.

(defmethod translate-item (item)
  (format nil "~A" item))

(defmethod translate-item ((item float))
  (format nil "~,,,,,,'eG" item))

(defmethod translate-item ((item pathname))
  (format nil "~A" (namestring item)))

(defun compile-char-lists (compilation cmd)
  (cond ((null cmd)
         (reverse
          (if (consp (first compilation))
              (cons (coerce (reverse (first compilation)) 'string) (rest compilation))
              compilation)))
        ((characterp (first cmd))
         (if (consp (first compilation))
             (compile-char-lists (cons (cons (first cmd) (first compilation))
                                       (rest compilation))
                                 (rest cmd))
             (compile-char-lists (cons (list (first cmd)) compilation)
                                 (rest cmd))))
        (t (compile-char-lists
            (cons (first cmd)
                  (if (consp (first compilation))
                      (cons (coerce (reverse (first compilation)) 'string)
                            (rest compilation))
                      compilation))
            (rest cmd)))))

(defstruct cmd-control
  predicate-mode
  (foreground t)
  (stderr-in-stdout t)
  (stderr :warn)
  error-unless-exit-code error-on-exit-code)

(defun int-char-p (char)
  (or (digit-char-p char)
      (member char '(#\- #\+))))

(defun parse-control-string (control-string)
  (let ((control (make-cmd-control)))
    (with-input-from-string (in control-string)
      (iter (for c in-stream in using 'read-char)
        (case c
          ((#\x #\X)
           (let ((! (eql #\! (peek-char nil in nil))))
             (when ! (read-char in nil))
             (let ((arg (let ((arg
                                (iter (while (ignore-errors
                                              (int-char-p
                                               (peek-char nil in nil))))
                                  (collect (read-char in nil) result-type 'string))))
                          (if (equal arg "")
                              0
                              (read-from-string arg)))))
               (if !
                   (setf (cmd-control-error-on-exit-code control) arg)
                   (setf (cmd-control-error-unless-exit-code control) arg)))))
          ((#\&) (setf (cmd-control-foreground control) nil))
          ((#\e #\E) (setf (cmd-control-stderr control) :error))
          ((#\w #\W) (setf (cmd-control-stderr control) :warn))
          ((#\p) (setf (cmd-control-predicate-mode control) t)))))
    control))

(defun |#>-reader| (stream subchar arg)
  """
The `#>' reader allows shell commands to be executed from Common Lisp programs.
Use shell commands as if they are normal Lisp functions (except see below as
how parameters are inserted into your shell commands).

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
         (control
           (parse-control-string
            (iter (until (equal #\( (peek-char nil stream t nil t)))
              (collect (read-char stream t nil t) result-type 'string))))
         ;; A stupid parser, all it need to do is match parentheses when things
         ;; can be escaped.
         (command
           (compile-char-lists
            nil
            (iter (for c = (read-char stream t nil t))
              (while c)
              (cond
                ;; Handle Lisp forms, this is really the only
                ;; complicated bit
                ((and (not escaped)
                      (eql c #\,))
                 (let ((sym (gensym))
                       (next (peek-char nil stream nil nil t)))
                   (case next
                     (#\@ (read-char stream nil nil t)
                      (push
                       (list sym `(apply #'mkdstr
                                         ,(read-preserving-whitespace
                                           stream t nil
                                           #+clisp nil
                                           #-clisp t)))
                       ext))
                     (otherwise
                      (let ((possible-delim
                              (read-char stream t nil t)))
                        (cond ((eql #\@
                                    (peek-char nil stream nil nil t))
                               ;; This is a delimiter
                               (read-char stream t nil t)
                               (push
                                (list sym `(apply
                                            #'mkdstr* ,possible-delim
                                            ,(read-preserving-whitespace
                                              stream t nil
                                              #+clisp nil
                                              #-clisp t)))
                                ext))
                              (t
                               ;; Concatenate the possible-delim
                               ;; character onto the beginning of
                               ;; the input as it is not, in, fact,
                               ;; a delimiter
                               (let ((stream (make-concatenated-stream
                                              (make-string-input-stream
                                               (mkstr possible-delim))
                                              stream)))
                                 (push
                                  (list sym (read-preserving-whitespace
                                             stream t nil
                                             #+clisp nil
                                             #-clisp t))
                                  ext)))))))
                   (setf c sym)))
                ;; Handle shell commands
                ((not (or quoted escaped))
                 (case c
                   (#\\ (setf escaped t))
                   (#\" (setf quoted #\"))
                   (#\' (setf quoted #\'))
                   (#\( (incf paren-level))
                   (#\) (decf paren-level))))
                ;; End quoted state if necessary
                ((eql quoted c)
                 (setf quoted nil))
                ;; End escaped state always on the next character
                ;; (which is this one)
                (escaped
                 (setf escaped nil))
                ;; This is here to catch the case where we have a
                ;; backslash in a quoted environment.
                ((eql #\\ c)
                 (setf escaped t)))
              (collect c)
              (while (> paren-level 0))
              (finally
               (when (eql #\/ (peek-char nil stream nil nil t))
                 (read-char stream t nil t)
                 (push (read stream t nil t) breaker)))))))
    `(let ,(mapcar (/. (x) (list (car x)
                                `(translate-item
                                  (let ((*shell-input* nil)) ,(cadr x)))))
                   (reverse ext))
       ,(let ((command-form
                `(cmd (if *remove-newlines*
                          (apply 'mkstr
                                 (mapcar
                                  (lambda (x)
                                    (if (stringp x)
                                        (substitute #\Space #\Newline x)
                                        x))
                                  ,(cons 'list command)))
                          (mkstr ,@command))
                      :wait ,(cmd-control-foreground control)
                      :output ,(if (cmd-control-foreground control)
                                   :string
                                   nil)
                      :error-on-exit-codes
                      ,(and (cmd-control-error-on-exit-code control)
                            `(list ,(cmd-control-error-on-exit-code control)))
                      :error-unless-exit-codes
                      ,(and (cmd-control-error-unless-exit-code control)
                            `(list ,(cmd-control-error-unless-exit-code control)))
                      :split-on ,(if breaker (apply #'mkstr breaker)))))
          command-form))))

(defun reader-wrapper (&rest args)
  (apply '|#>-reader| args))

(set-dispatch-macro-character #\# #\> 'reader-wrapper)
