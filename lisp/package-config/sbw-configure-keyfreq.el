(require 'use-package)

(use-package keyfreq
  :init
  (progn
    (keyfreq-mode 1)
    (setq keyfreq-autosave-mode 1)))

(provide 'sbw-configure-keyfreq)
