
(defpackage :clommand-test
  (:use :cl :stefil :iterate)
  (:export #:shell-test))

(in-package :clommand-test)

(in-root-suite)

(defsuite* shell-test)

(deftest read-write ()
  (with-open-file (out #p"test-output" :direction :output :if-exists :supersede)
    (iter (for line in #>(cat test-input)/#\Newline)
      (format out "~A~%" line) ))
  (is (equal "" #>(diff test-input test-output)))
  (is (equal (with-input-from-string (str #>(wc test-input))
               (list (read str) (read str) (read str)) )
             (list (length #>(cat test-input)/#\Newline)
                   (length #>(cat test-input)/"\\s+")
                   (length #>(cat test-input)) ))))

;; This walks a directory hierarchy and finds and .git directories, then runs a
;; git diff on them to determine if they are up to date with the remote branch.
;; Useful for determining if your collection of Lisp systems are synced with
;; origin.
(iter (for repo in #>(find ~/src/lisp/pkg/ -name .git)/#\Newline)
      (let ((*default-pathname-defaults* (pathname repo)))
        (collect (list repo #>(git diff))) ))

