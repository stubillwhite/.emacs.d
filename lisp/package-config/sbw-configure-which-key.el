(require 'use-package)

(use-package which-key
  :init
  (progn
    (which-key-setup-side-window-right)

    (setq
      which-key-special-keys nil ;; No keys are special, display long versions of SPC, TAB, RET, etc
      )))

(provide 'sbw-configure-which-key)
