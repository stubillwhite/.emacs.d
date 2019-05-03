(use-package diminish
  :config
  (progn
    (eval-after-load "org-indent" '(diminish 'org-indent-mode))
    (eval-after-load "mw-thesaurus" '(diminish 'mw-thesaurus-mode))))

(provide 'sbw-configure-diminish)
