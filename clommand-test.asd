
(asdf:defsystem #:clommand-test
  :name "clommand-test"
  :author "Zachary Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description
  "Tests for Clommand"
  :components ((:file "test"))
  :serial t
  :depends-on (:stefil :iterate :clommand))
