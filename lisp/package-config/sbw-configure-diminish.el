(use-package diminish
  :config
  (progn
    (eval-after-load "org-indent" '(diminish 'org-indent-mode))
    (eval-after-load "mw-thesaurus" '(diminish 'mw-thesaurus-mode))
    (eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
    ))

(provide 'sbw-configure-diminish)
