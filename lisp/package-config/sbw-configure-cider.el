(use-package cider
  :straight t
  ;;:straight (cider :type git :files ("*.el" (:exclude ".dir-locals.el")) :host github :repo "clojure-emacs/cider" :branch "62de7dc372ae1c7e2bee32a30c8b45c604a14f44")

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
    (add-hook 'cider-mode-hook 'turn-on-eldoc-mode)

    ;; General settings
    (setq 
     cider-lein-parameters                "repl :headless :host localhost" ;; Jack-in parameters
     cider-auto-select-error-buffer        nil                             ;; Don't auto-select the error buffer when displayed
     cider-popup-stacktraces               nil                             ;; Suppress the error buffer pop up in buffers other than the REPL
     cider-repl-display-in-current-window  t                               ;; C-c C-z switch to the REPL
     cider-repl-history-size               3000                            ;; Longer history
     cider-repl-pop-to-buffer-on-connect   nil                             ;; Suppress auto-display of the REPL buffer on connection
     cider-repl-popup-stacktraces          nil                             ;; Don't allow the the error buffer to pop up in the REPL
     cider-repl-print-length               100                             ;; Limit the items of collections to print
     cider-repl-result-prefix              ";; => "                        ;; Comment prefix for results in the REPL
     cider-repl-use-clojure-font-lock      t                               ;; Prettier fonts in the REPL
     cider-repl-use-pretty-printing        nil                             ;; Pretty print results in the REPL
     cider-repl-wrap-history               t                               ;; Wrap history
     nrepl-log-messages                    t                               ;; Log messages to aid debug of CIDER problems
     nrepl-port                            "4555"                          ;; Default port number
     cider-default-cljs-repl               'figwheel-main
     cider-figwheel-main-default-options   "dev"
     )

    ;; Leiningen
    (setq cider-lein-command
          (if (sbw/is-darwin?)
              "/usr/local/bin/lein"
            "~/tools/bin/lein"))

    ;; Boot
    (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
    (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
    
    ;; Better naming for the REPL buffer
    (setq 
     nrepl-buffer-name-separator "-"
     nrepl-buffer-name-show-port t)

    ;; Helper functions

    (defun sbw/cider-reset-repl ()
      "Open the REPL window if it not currently open, switch focus to it, and reset."
      (interactive)
      (save-some-buffers t)
      (sbw/open-and-switch-to-window (cider-current-repl-buffer))
      (cider-insert-in-repl "(ns user)" t)
      (cider-insert-in-repl "(reset)" t))

    (defun sbw/cider-refresh-repl ()
      "Open the REPL window if it not currently open, switch focus to it, and refresh."
      (interactive)
      (save-some-buffers t)
      (sbw/open-and-switch-to-window (cider-current-repl-buffer))
      (cider-repl-clear-buffer)
      (cider-insert-in-repl "(ns user)" t)
      (cider-insert-in-repl "(refresh-all)" t)
      (cider-insert-in-repl "(reset)" t))

    (defun sbw/cider-org-mode-jack-in ()
      "Start the org-mode-babel REPL."
      (interactive)
      (with-current-buffer (find-file (concat org-directory "/project.clj"))
        (cider-jack-in))))

  :bind
  (:map cider-mode-map
        ("TAB"    . cider-repl-indent-and-complete-symbol)
        ("<f5>"   . sbw/cider-reset-repl)
        ("C-<f5>" . sbw/cider-refresh-repl)))

(provide 'sbw-configure-cider)
