(require 'use-package)

(use-package company
  :diminish company-mode
  
  :init
  (progn
    (global-company-mode))

  :config
  (progn
    (setq
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase    nil)))

(provide 'sbw-configure-company)
