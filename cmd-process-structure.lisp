
(in-package :clommand)

;; @The command structure, <<cmd-process>>, holds the relavant information of a
;; running process.  The <input>, <output>, and <error> slots hold the
;; appropriate streams (where appropriate) for the various input and output
;; streams.  The implementation's underlying process structure, if one is
;; defined, is held in the <process-obj> slot.  The structure may have more
;; slots depending on the needs of the implementation.

;;<<>>=
(defstruct cmd-process
  input output error process-obj)

;; @You can also access a status of the process using <<cmd-process-status>>,
;; which will return a symbol or number describing the current state of the
;; process.  This status will be one of: <:running>, <:stopped> or an integer
;; exit value.

;;<<>>=
(defun cmd-process-status (process)
  #+sbcl
  (let ((status (if (cmd-process-p (cmd-process-process-obj process))
                    ;; Sometimes they are nested
                    (sb-ext:process-status
                     (cmd-process-process-obj
                      (cmd-process-process-obj process)))
                    (sb-ext:process-status
                     (cmd-process-process-obj process)))))
    (case status
      ((:stopped :running) status)
      (otherwise
       (sb-ext:process-exit-code
        (cmd-process-process-obj process)))))
  #+cmu
  (let ((status (if (cmd-process-p (cmd-process-process-obj process))
                    ;; Sometimes they are nested
                    (ext:process-status
                     (cmd-process-process-obj
                      (cmd-process-process-obj process)))
                    (ext:process-status
                     (cmd-process-process-obj process)))))
    (case status
      ((:stopped :running) status)
      (otherwise
       (ext:process-exit-code
        (cmd-process-process-obj process))))))

;;<<>>=
#+sbcl

#+cmu
(defun cmd-process-exit-code (process)
  (ext:process-wait (cmd-process-process-obj process))
  (ext:process-exit-code (cmd-process-process-obj process)))
#+ccl


;; @This is a user visible structure when the process is run in the background
;; via <<cmd-bg>>.



