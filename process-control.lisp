
;; @The idea here is that we could manage our processes in a pretty
;; sophisticated way from Lisp using signals.  This would include thinks like
;; stopping and restarting processes.  The debugger wrappers below where
;; prototypes experimenting in how we might be able to stop precesses when the
;; debugger is invoked and restart them when a continuing restart is called or
;; kill them when an abort restart is called.

;; This doesn't really work, at least not in SBCL, and SBCL is the only place I
;; tried.  The issue with SBCL is that the process control on the Lisp side
;; seems to miss out on many of the changes that happen due to signals I am
;; sending via <sb-ext:process-kill>.

(defun cmd-debugger-wrapper (cond debugger)
  (declare (ignore debugger))
  (print 'stop)
  (%cmd (%mkstr "kill -SIGSTOP "
                (print (sb-ext:process-pid
                        (cmd-process-process-obj (first *jobs*)))))
        nil nil nil)
  (restart-case
      (invoke-debugger cond)
    (abort ()
      (print 'cont)
      (%cmd (%mkstr "kill -SIGCONT "
                    (print (sb-ext:process-pid
                            (cmd-process-process-obj (first *jobs*)))))
            nil nil nil)
      (print 'term)
      (%cmd (%mkstr "kill -SIGTERM "
                    (print (sb-ext:process-pid
                            (cmd-process-process-obj (first *jobs*)))))
            nil nil nil)
      (sb-posix:waitpid (sb-ext:process-pid
                         (cmd-process-process-obj (first *jobs*)))
                        0)
      (abort))
    (continue ()
      (print 'cont)
      (%cmd (%mkstr "kill -SIGCONT "
                    (print (sb-ext:process-pid
                            (cmd-process-process-obj (first *jobs*)))))
            nil nil nil)
      (continue))))

(defun cmd-debugger-wrapper (cond debugger)
  (declare (ignore debugger))
  (print 'stop)
  (sb-ext:process-kill
   (cmd-process-process-obj (first *jobs*))
   sb-posix:sigstop)
  (restart-case
      (invoke-debugger cond)
    (abort ()
      (print 'cont)
      (sb-ext:process-kill
       (cmd-process-process-obj (first *jobs*))
       sb-posix:sigcont)
      (print 'term)
      (sb-ext:process-kill
       (cmd-process-process-obj (first *jobs*))
       sb-posix:sigterm)
      (sb-ext:process-wait (cmd-process-process-obj (first *jobs*)))
      ;; (print 'kill)
      ;; (sb-ext:process-kill
      ;;  (cmd-process-process-obj (first *jobs*))
      ;;  sb-posix:sigkill)
      (abort))
    (continue ()
      (print 'cont)
      (sb-ext:process-kill
       (cmd-process-process-obj (first *jobs*))
       sb-posix:sigcont)
      (continue))))