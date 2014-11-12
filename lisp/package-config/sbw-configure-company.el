(require 'use-package)

(use-package company
  :init
  (progn
    (global-company-mode))

  :config
  (progn)

  :bind
  ("C-TAB" . company-complete)
  )

(provide 'sbw-configure-company)
