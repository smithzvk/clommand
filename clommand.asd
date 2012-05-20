
(asdf:defsystem #:clommand
  :name "clommand"
  :author "Zachary Smith <elzacho@gmail.com>"
  :license "GPLv3 or later"
  :description
  "A package for using the shell easily and uniformly from Common Lisps"
  :components ((:file "package")
               (:file
                #+sbcl "cmd-sbcl"
                #+cmu "cmd-cmucl"
                #+clisp "cmd-clisp"
                #+ecl "cmd-ecl"
                #+ccl "cmd-ccl"
                #+abcl "cmd-abcl")
               (:file "cmd")
               (:file "reader-macro"))
  :serial t
  :depends-on (:toolbox :iterate :cl-fad :cl-ppcre))
