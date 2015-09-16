(require 'use-package)

(use-package which-key
  :init
  (progn
    (setq
      which-key-special-keys nil ;; No keys are special, display long versions of SPC, TAB, RET, etc
      )
    
    (which-key-setup-side-window-right)
    (which-key-mode)))

(provide 'sbw-configure-which-key)
