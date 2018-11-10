(use-package beacon
  :diminish beacon-mode
  
  :init
  (progn
    (beacon-mode 1)
    
    (add-hook 'after-init-hook
              (lambda () (setq beacon-color (face-background 'match nil t)
                          beacon-size  100)))

    (defun sbw/beacon-mode-debug ()
      (interactive)
      ;; (message (format (string-join (list "beacon-mode %s" "becon--timer %s") "\n") beacon-mode (timerp beacon--timer)))
      (trace-function 'beacon--shine)
      (trace-function 'beacon--dec)
      (trace-function 'beacon--vanish)
      (trace-function 'beacon--blink)
      )

    (defun sbw/beacon-mode-debug-off ()
      (interactive)
      (untrace-all)))

  :bind
  ("<f6>" . sbw/beacon-mode-debug)
  ("<S-f6>" . sbw/beacon-mode-debug-off))

(provide 'sbw-configure-beacon)

