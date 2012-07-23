
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
  (let ((status (sb-ext:process-status
                 (cmd-process-process-obj process))))
    (case status
      ((:stopped :running) status)
      (otherwise
       (sb-ext:process-exit-code
        (cmd-process-process-obj process))))))

;;<<>>=
(defun cmd-process-exit-code (process)
  (sb-ext:process-wait (cmd-process-process-obj process))
  (sb-ext:process-exit-code (cmd-process-process-obj process)))

;; @This is a user visible structure when the process is run in the background
;; via <<cmd-bg>>.



