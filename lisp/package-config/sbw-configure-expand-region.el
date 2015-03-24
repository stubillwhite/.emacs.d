(require 'use-package)

(use-package expand-region
  :config
  (progn)

  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(provide 'sbw-configure-expand-region)
