(require 'use-package)

(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.css?\\'"  . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    ))

(provide 'sbw-configure-web-mode)
