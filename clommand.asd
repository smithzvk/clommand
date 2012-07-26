
(asdf:defsystem #:clommand
  :name "clommand"
  :author "Zachary Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description
  "A package for using the shell easily and uniformly from Common Lisps"
  :components ((:file "package")
               (:file "string-helpers")
               #+ecl (:file "cmd-ecl")
               #-ecl (:file
                #+sbcl "cmd-sbcl"
                #+cmu "cmd-cmucl"
                #+clisp "cmd-clisp"
                #+ecl "cmd-ecl"
                #+ccl "cmd-ccl"
                #+abcl "cmd-abcl")
               (:file "cmd")
               (:file "reader-macro"))
  :serial t
  :depends-on (:iterate :cl-fad :cl-ppcre :cl-plumbing
                :protected-bindings :pythonic-string-reader))
