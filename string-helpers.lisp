
(in-package :clommand)

;; Some string utilities

(defun ~/ (&rest args)
  (concatenate
   'string
   "$HOME/" (apply #'./ (rest args))))

(defun /./ (&rest args)
  (concatenate
   'string
   "/" (apply #'./ (rest args))))

(defgeneric stringify (obj)
  (:method (obj)
    (with-output-to-string (out)
      (princ obj out))))

(defun ./ (&rest args)
  (if (null (rest args))
      (first args)
      (concatenate
       'string
       (first args)
       "/"
       (apply #'./ (rest args)))))

(defvar _ " ")

(defun %mkdstr (delim &rest args)
  "Make delimited string"
  (with-output-to-string (out)
    (iter (for a in args)
      (unless (first-iteration-p)
        (princ (stringify delim) out))
      (princ a out))))

(defun %mkstr (&rest args)
  "Make string"
  (with-output-to-string (out)
    (iter (for a in args)
      (princ a out))))

(defun wrap-in-{} (string)
  (concatenate 'string "{ " string "; }"))
