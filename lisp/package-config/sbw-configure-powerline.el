(require 'use-package)

(use-package powerline
  :init
  (progn
    (powerline-default-theme)
    (setq powerline-default-separator 'wave)

    ;; TODO: Tidy this up, should be in diminish configuration
    (require 'diminish)
    (diminish 'company-mode)
    (diminish 'helm-mode)
    (diminish 'undo-tree-mode)
    (diminish 'smartparens-mode))

  :config
  (progn))

(provide 'sbw-configure-powerline)
