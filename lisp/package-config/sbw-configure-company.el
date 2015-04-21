(require 'use-package)

(use-package company
  :diminish company-mode
  
  :init
  (progn
    (global-company-mode))

  :config
  (progn))

(provide 'sbw-configure-company)
