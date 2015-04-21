(require 'use-package)

(use-package diminish
  :config
  (progn
    (eval-after-load "Org-Indent" '(diminish 'org-indent-mode))
    (eval-after-load "Magit"      '(diminish 'magit-auto-revert-mode))
    ))

(provide 'sbw-configure-diminish)
