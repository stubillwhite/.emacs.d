(require 'use-package)

(use-package web-mode
  :init
  (progn
    ;; File types
    (add-to-list 'auto-mode-alist '("\\.css?\\'"  . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

    ;; Indentation
    (setq
      web-mode-markup-indent-offset 4
      web-mode-css-indent-offset    4
      web-mode-code-indent-offset   4)
    ))

(provide 'sbw-configure-web-mode)
