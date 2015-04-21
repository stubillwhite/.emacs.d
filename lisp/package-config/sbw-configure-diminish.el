(require 'use-package)

(use-package diminish
  :config
  (progn
    (eval-after-load
      "Org-Indent"
      '(diminish 'org-indent-mode))))

(provide 'sbw-configure-diminish)
