
(asdf:defsystem #:clommand
  :name "clommand"
  :author "Zachary Smith <elzacho@gmail.com>"
  :license "GPLv3 or later"
  :description
  "A package for using the shell easily and uniformly from Common Lisps"
  :components ((:file "package")
               (:file "cmd") )
  :serial t
  :depends-on (:toolbox :iterate :cl-fad :cl-ppcre) )
