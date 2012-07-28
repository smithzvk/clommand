
(defpackage :clommand-test
  (:use :cl :stefil :iterate :clommand :cl-ppcre)
  (:export #:simple-commands-reader-macro
           #:simple-commands
           #:run-tests
           #:background-commands))

(in-package :clommand-test)

(cl-interpol:enable-interpol-syntax)

(in-root-suite)

(defsuite* shell-test)

;; @In this test suite, we necessarily have to assume that you have a small set
;; of reasonably standards compliant Unix utilities installed.  These utilities
;; are:

;; \begin{enumerate}

;; \item cat
;; \item grep
;; \item wc

;; \end{enumerate}

(defun run-tests ()
  (simple-commands)
  (simple-commands-reader-macro))

(deftest simple-commands ()
  (let ((*default-pathname-defaults*
          (asdf:component-pathname
           (asdf:find-system :clommand-test))))
    ;; Controlling string output
    (is (equal '("test" "" 0)
               (multiple-value-list (cmd "echo test"))))
    (is (equal '("" "" 0)
               (multiple-value-list (cmd "echo test" :output nil))))
    (is (equal '("" "test" 0)
               (multiple-value-list (cmd "echo test" :output :error))))
    (is (equal '("" "" 0)
               (multiple-value-list (cmd "echo test" :output :error :error nil))))
    (is (equal '("" "bash: this-does-not-exist: command not found" 127)
               (multiple-value-list (cmd "this-does-not-exist"))))
    (is (equal '("bash: this-does-not-exist: command not found" "" 127)
               (multiple-value-list (cmd "this-does-not-exist" :error :output))))
    (is (equal '("test" "bash: this-does-not-exist: command not found" 127)
               (multiple-value-list (cmd "echo test && this-does-not-exist"))))
    (is (equal '("bash: this-does-not-exist: command not found" "test" 127)
               (multiple-value-list (cmd "echo test && this-does-not-exist"
                                         :output :error
                                         :error :output))))
    (is (equal '(#?"test\nbash: this-does-not-exist: command not found" "" 127)
               (multiple-value-list (cmd "echo test && this-does-not-exist"
                                         :output t :error :output))))
    (is (equal '("" #?"test\nbash: this-does-not-exist: command not found" 127)
               (multiple-value-list (cmd "echo test && this-does-not-exist"
                                         :output :error :error t))))
    ;; Error on return value
    (is (eql :correct (handler-case
                          (cmd "this-does-not-exist" :error-on-exit-codes '(127))
                        (error () :correct))))
    (is (eql :correct (handler-case
                          (cmd "this-does-not-exist" :error-unless-exit-codes '(0))
                        (error () :correct))))
    ;; Error on output to standard error
    (is (eql :correct (handler-case
                          (cmd "this-does-not-exist" :on-error-output :error)
                        (error () :correct))))
    (is (eql :correct (handler-case
                          (cmd "this-does-not-exist" :on-error-output :warn)
                        (warning () :correct))))
    ;; piping

    ;; splitting
    ))

(deftest simple-commands-reader-macro ()
  (let ((*default-pathname-defaults*
          (asdf:component-pathname
           (asdf:find-system :clommand-test))))
    ;; Controlling string output
    (is (equal '("test" "" 0)
               (multiple-value-list #>(echo test))))
    (is (equal '("" "bash: this-does-not-exist: command not found" 127)
               (multiple-value-list #>w(this-does-not-exist))))
    (is (equal '("bash: this-does-not-exist: command not found" "" 127)
               (multiple-value-list #>o(this-does-not-exist))))
    (is (equal '("test" "bash: this-does-not-exist: command not found" 127)
               (multiple-value-list #>w(echo test && this-does-not-exist))))

    (is (equal '("test\nbash: this-does-not-exist: command not found" "" 127)
               (multiple-value-list #>o(echo test && this-does-not-exist))))
    ;; Error on return value
    (is (eql :correct (handler-case
                          #>x!127(this-does-not-exist)
                          (error () :correct))))
    (is (eql :correct (handler-case
                          #>x0(this-does-not-exist)
                          (error () :correct))))
    ;; Error on output to standard error
    (is (eql :correct (handler-case
                          #>e(this-does-not-exist)
                          (error () :correct))))
    (is (eql :correct (handler-case
                          #>w(this-does-not-exist)
                          (warning () :correct))))
    ;; splitting

    ))

(defun slurp-stream (stream)
  "Slurp the output of a stream."
  (coerce
   (iter (for c in-stream stream using #'read-char)
     (collecting c result-type 'vector))
   'string))

(defun slurp-process-output (process)
  "Take a process structure and return the output as it would appear if it ran
in the foreground."
  (list (slurp-stream (clommand::cmd-process-output process))
        (slurp-stream (clommand::cmd-process-error process))
        (clommand::cmd-process-exit-code process)))

(deftest background-commands ()
  (let ((*default-pathname-defaults*
          (asdf:component-pathname
           (asdf:find-system :clommand-test))))
    ;; Controlling string output
    (is (equal '(#?"test\n" "" 0)
               (slurp-process-output (cmd-bg "echo test"))))
    (is (equal '("" "" 0)
               (slurp-process-output (cmd-bg "echo test" :output nil))))
    (is (equal '("" #?"test\n" 0)
               (slurp-process-output (cmd-bg "echo test" :output :error))))
    (is (equal '("" "" 0)
               (slurp-process-output (cmd-bg "echo test" :output :error :error nil))))
    (is (equal '("" #?"bash: this-does-not-exist: command not found\n\n" 127)
               (slurp-process-output (cmd-bg "this-does-not-exist"))))
    (is (equal '(#?"bash: this-does-not-exist: command not found\n\n" "" 127)
               (slurp-process-output (cmd-bg "this-does-not-exist" :error :output))))
    (is (equal '(#?"test\n" #?"bash: this-does-not-exist: command not found\n\n" 127)
               (slurp-process-output (cmd-bg "echo test && this-does-not-exist"))))
    (is (equal '(#?"bash: this-does-not-exist: command not found\n\n" #?"test\n" 127)
               (slurp-process-output (cmd-bg "echo test && this-does-not-exist"
                                             :output :error
                                             :error :output))))
    ;; (is (equal '(#?"test\nbash: this-does-not-exist: command not found" "" 127)
    ;;            (slurp-process-output (cmd "echo test && this-does-not-exist"
    ;;                                      :output t :error :output))))
    ;; (is (equal '("" #?"test\nbash: this-does-not-exist: command not found" 127)
    ;;            (slurp-process-output (cmd "echo test && this-does-not-exist"
    ;;                                      :output :error :error t))))

    ;; ;; Error on return value
    ;; (is (eql :correct (handler-case
    ;;                       (cmd "this-does-not-exist" :error-on-exit-codes '(127))
    ;;                     (error () :correct))))
    ;; (is (eql :correct (handler-case
    ;;                       (cmd "this-does-not-exist" :error-unless-exit-codes '(0))
    ;;                     (error () :correct))))
    ;; ;; Error on output to standard error
    ;; (is (eql :correct (handler-case
    ;;                       (cmd "this-does-not-exist" :on-error-output :error)
    ;;                     (error () :correct))))
    ;; (is (eql :correct (handler-case
    ;;                       (cmd "this-does-not-exist" :on-error-output :warn)
    ;;                     (warning () :correct))))

    ;; piping
    ;; return value

    ;; splitting
    ))

(deftest output-options ()
  )

(deftest input-options ()
  )

(deftest reader-macro-tests ()
  (let ((*default-pathname-defaults*
          (asdf:component-pathname
           (asdf:find-system :clommand-test))))
    ;; simple command
    (is (equal
         "59  602 3998 test-input"
         #>(wc "test-input")))
    ;; piping
    (is (equal
         "10     104     700"
         #>(tail "test-input" | wc | cat)))

    ;; return value
    #>x(cat test-input)

    ;; splitting
    #>/"\\n"(cat test-input)

    ;; predicate mode
    ))

(deftest read-write ()
  (let ((*default-pathname-defaults*
          (asdf:component-pathname
           (asdf:find-system :clommand-test))))
    (with-open-file (out #p"test-output" :direction :output :if-exists :supersede)
      (iter (for line in (split #\Newline #>(cat test-input)))
        (format out "~A~%" line)))
    (is (equal "" #>(diff test-input test-output)))
    (is (equal (with-open-file (in #p"test-input")
                 (file-length in))
               (first
                (last
                 (mapcar #'read-from-string
                         (split "\\s+" #>(wc test-input))) 2))))))

;; (deftest piping ()
;;   (let ((*default-pathname-defaults*
;;           (asdf:component-pathname
;;            (asdf:find-system :clommand-test))))
;;     ()))

;; (deftest redirected-output ())

;; (deftest redirected-input ())

;; (deftest split-output ()
;;   "This test the facilities for splitting output from shell commands."
;;   (iter (for line in #>(cat test-input)/#\Newline)
;;     (format out "~A~%" line)))

;; (deftest multiline ())

;; (deftest parameter-insertion ())

;; (deftest output-conversion ())

;; (deftest background-process ())

;; (deftest interactive-process ())

;; Return value of 1 is expected, anything else is an error.  
;; #>(true)

;; (cmd)

;; (cmd-p)

;; This walks a directory hierarchy and finds and .git directories, then runs a
;; git diff on them to determine if they are up to date with the remote branch.
;; Useful for determining if your collection of Lisp systems are synced with
;; origin.

;; (iter (for repo in #>(find ~/src/lisp/pkg/ -name .git)/#\Newline)
;;       (let ((*default-pathname-defaults* (pathname repo)))
;;         (collect (list repo #>(git diff origin)))))

