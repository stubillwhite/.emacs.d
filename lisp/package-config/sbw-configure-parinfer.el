(use-package parinfer
  :init
  (progn
    (setq
     parinfer-extensions '( defaults      ; should be included.
                            pretty-parens ; different paren styles for different modes.
                            evil          ; If you use Evil.
                            lispy ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
                            paredit     ; Introduce some paredit commands.
                            smart-tab   ; C-b & C-f jump positions and smart shift with tab & S-tab.
                            smart-yank)) ; Yank behavior depend on mode.

    (setq parinfer-auto-switch-indent-mode-when-closing t ;; Auto-indent when balanced
          ) 
    
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode))

  :bind
  (("C-," . parinfer-toggle-mode)))
