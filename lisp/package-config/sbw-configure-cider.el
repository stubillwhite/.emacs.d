(require 'use-package)

(use-package cider
  :defer nil ;; Deferring interferes with jacking in
  
  :init
  (progn)

  :config
  (progn
    ;; Autocomplete using company-mode
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    
    ;; Hide DOS EOL characters in the REPL
    (add-hook 'cider-repl-mode-hook 'sbw/hide-dos-eol)

    ;; Enable eldoc in Clojure buffers
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

    ;; General settings
    (setq 
      cider-auto-select-error-buffer        nil      ;; Don't auto-select the error buffer when displayed
      cider-popup-stacktraces               nil      ;; Suppress the error buffer pop up in buffers other than the REPL
      cider-repl-display-in-current-window  t        ;; C-c C-z switch to the REPL
      cider-repl-history-size               3000     ;; Longer history
      cider-repl-pop-to-buffer-on-connect   nil      ;; Suppress auto-display of the REPL buffer on connection
      cider-repl-popup-stacktraces          nil      ;; Don't allow the the error buffer to pop up in the REPL
      cider-repl-print-length               100      ;; Limit the items of collections to print
      cider-repl-result-prefix              ";; => " ;; Comment prefix for results in the REPL
      cider-repl-use-clojure-font-lock      t        ;; Prettier fonts in the REPL
      cider-repl-use-pretty-printing        nil      ;; Pretty print results in the REPL
      cider-repl-wrap-history               t        ;; Wrap history
      nrepl-log-messages                    t        ;; Log messages to aid debug of CIDER problems
      nrepl-port                            "4555"   ;; Default port number
      )

    (setq cider-lein-command "~/tools/bin/lein")

    ;; Better naming for the REPL buffer
    (setq 
      nrepl-buffer-name-separator "-"
      nrepl-buffer-name-show-port t)

    ;; Helper functions

    (defun sbw/cider-switch-to-repl-buffer ()
      "Open the REPL window if it not currently open and switch focus to it."
      (let* ( (repl-buffer (cider-current-repl-buffer)) )
        (when (not (eq (current-buffer) repl-buffer))
          (if (get-buffer-window repl-buffer)
            (progn
              (switch-to-buffer-other-window repl-buffer))
            (progn
              (split-window-right)
              (switch-to-buffer-other-window (buffer-name))
              (switch-to-buffer repl-buffer))))))

    (defun sbw/cider-reset-repl ()
      "Open the REPL window if it not currently open, switch focus to it, and reset."
      (interactive)
      (save-some-buffers t)
      (sbw/cider-switch-to-repl-buffer)
      (cider-insert-in-repl "(ns user)" t)
      (cider-insert-in-repl "(reset)" t))

    (defun sbw/cider-refresh-repl ()
      "Open the REPL window if it not currently open, switch focus to it, and refresh."
      (interactive)
      (save-some-buffers t)
      (sbw/cider-switch-to-repl-buffer)
      (cider-repl-clear-buffer)
      (cider-insert-in-repl "(ns user)" t)
      (cider-insert-in-repl "(refresh-all)" t)
      (cider-insert-in-repl "(reset)" t))

    (defun sbw/cider-jack-in-and-go ()
      (interactive)
      (save-some-buffers t)
      (sbw/cider-switch-to-repl-buffer)
      (cider-insert-in-repl "(ns user)")
      (cider-insert-in-repl "(go)"))

    (bind-keys :map cider-mode
      ("TAB"    . cider-repl-indent-and-complete-symbol)
      ("<f5>"   . sbw/cider-reset-repl)
      ("C-<f5>" . sbw/cider-refresh-repl)))
  )

(provide 'sbw-configure-cider)
