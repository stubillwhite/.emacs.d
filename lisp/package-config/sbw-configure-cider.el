(require 'use-package)

(use-package cider
  :defer t
  
  :init
  (progn)
  
  :config
  (progn
    ;; Hide DOS EOL characters in the REPL
    (add-hook 'cider-repl-mode-hook 'sbw/hide-dos-eol)

    ;; Enable eldoc in Clojure buffers
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

    ;; General settings
    (setq 
      nrepl-hide-special-buffers            t ;; Hide special buffers from buffer menus
      cider-repl-pop-to-buffer-on-connect   nil ;; Suppress auto-display of the REPL buffer on connection
      cider-popup-stacktraces               nil ;; Suppress the error buffer pop up in buffers other than the REPL
      cider-repl-popup-stacktraces          nil ;; Don't allow the the error buffer to pop up in the REPL
      cider-auto-select-error-buffer        nil ;; Don't auto-select the error buffer when displayed
      cider-repl-display-in-current-window  t   ;; C-c C-z switch to the CIDER REPL
      cider-repl-print-length               100 ;; Limit the items of collections to print
      )

    ;; Better naming for the REPL buffer
    (setq 
      nrepl-buffer-name-separator "-"
      nrepl-port                  "4555"
      nrepl-buffer-name-show-port t)

    ;; Helper functions

    (defun sbw/cider-switch-to-repl-buffer ()
      "Open the REPL window if it not currently open and switch focus to it."
      (let* ( (repl-buffer (cider-current-repl-buffer)) )
        (if (not (get-buffer-window repl-buffer))
          (progn
            (split-window-below 15)
            (switch-to-buffer repl-buffer))
          (switch-to-buffer-other-window repl-buffer))))

    (defun sbw/cider-reset-repl ()
      "Open the REPL window if it not currently open, switch focus to it, and reset."
      (interactive)
      (save-some-buffers t)
      (sbw/cider-switch-to-repl-buffer)
      (cider-find-and-clear-repl-buffer)
      (cider-insert-in-repl "(ns user)" t)
      (cider-insert-in-repl "(reset)" t))

    (defun sbw/cider-refresh-repl ()
      "Open the REPL window if it not currently open, switch focus to it, and refresh."
      (interactive)
      (save-some-buffers t)
      (sbw/cider-switch-to-repl-buffer)
      (cider-find-and-clear-repl-buffer)
      (cider-insert-in-repl "(ns user)" t)
      (cider-insert-in-repl "(refresh)" t)
      (cider-insert-in-repl "(reset)" t))))

(provide 'sbw-configure-cider)
