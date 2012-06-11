
(defpackage :clommand-test
  (:use :cl :stefil :iterate :clommand :cl-ppcre)
  (:export #:shell-test))

(in-package :clommand-test)

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

(deftest simple-commands ()
  (let ((*default-pathname-defaults*
          (asdf:component-pathname
           (asdf:find-system :clommand-test))))
    (is (equal
         "59  602 3998 test-input"
         #>(wc "test-input")))
    (is (equal
         "10     104     700"
         #>(tail "test-input" | wc | cat)))
    (is (equal
         "10     107     708"
         #>(head "test-input" | wc | cat)))))

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

