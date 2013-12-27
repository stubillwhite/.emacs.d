(require 'cider)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; General settings
(setq 
  nrepl-hide-special-buffers            t       ;; Hide special buffers from buffer menus
  cider-repl-pop-to-buffer-on-connect   nil     ;; Suppress auto-display of the REPL buffer on connection
  cider-popup-stacktraces               nil     ;; Suppress the error buffer pop up in buffers other than the REPL
  cider-repl-popup-stacktraces          t       ;; Enable the error buffer pop up in the REPL
  cider-auto-select-error-buffer        nil     ;; Don't auto-select the error buffer when displayed
  cider-repl-display-in-current-window  t       ;; C-c C-z switch to the CIDER REPL
  cider-repl-print-length               100     ;; Limit the items of collections to print
  )

;; Better naming for the REPL buffer
(setq 
  nrepl-buffer-name-separator "-"
  nrepl-buffer-name-show-port t)

(provide 'sbw-setup-cider)
