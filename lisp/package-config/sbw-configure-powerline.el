(require 'use-package)

(use-package powerline
  :init
  (progn
    (powerline-default-theme)
    (setq powerline-default-separator 'wave))

  :config
  (progn))

(provide 'sbw-configure-powerline)
