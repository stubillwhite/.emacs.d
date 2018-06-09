(use-package diminish
  :config
  (progn
    (eval-after-load "org-indent" '(diminish 'org-indent-mode))))

(provide 'sbw-configure-diminish)
