
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
;;                                                   (pathname line) ))))))

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
