(use-package which-key
  :diminish which-key-mode
  
  :init
  (progn
    (which-key-setup-side-window-right)
    (which-key-mode)

    ;; No keys are special, display long versions of SPC, TAB, RET, etc
    (setq which-key-special-keys nil)))

(provide 'sbw-configure-which-key)
