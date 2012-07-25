
(defpackage :clommand
    (:nicknames :cmd)
  (:use :cl :iter :fad :protected-bindings)
  (:export #:cmd
           #:cmd-p
           #:cmd-bg))

(pythonic-string-reader:enable-pythonic-string-syntax)
