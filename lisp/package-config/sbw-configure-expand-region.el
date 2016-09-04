(require 'use-package)

(use-package expand-region
  :config
  (progn)

  :bind
  ("M-<up>" . er/expand-region)
  ("M-<down>" . er/contract-region))

(provide 'sbw-configure-expand-region)
