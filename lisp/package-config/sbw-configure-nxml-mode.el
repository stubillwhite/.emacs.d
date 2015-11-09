(require 'use-package)
(require 'use-package)

(use-package nxml-mode
  :config
  (progn
    (setq
     tab-width         4
     nxml-child-indent 4
     )

    (add-hook 'nxml-mode-hook (lambda () (flyspell-mode nil)))
    ))

(provide 'sbw-configure-nxml-mode)
