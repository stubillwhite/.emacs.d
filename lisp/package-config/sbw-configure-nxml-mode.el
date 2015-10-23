(require 'use-package)
(require 'use-package)

(use-package nxml-mode
  :config
  (progn
    (setq
     tab-width         4
     nxml-child-indent 4
     )))

(provide 'sbw-configure-nxml-mode)
