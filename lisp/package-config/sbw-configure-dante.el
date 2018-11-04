(use-package dante
  :diminish t

  :commands 'dante-mode

  :init
  (progn
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'haskell-mode-hook 'flycheck-mode)

    ;; Use hlint
    (add-hook 'dante-mode-hook '(lambda () (flycheck-add-next-checker
                                       'haskell-dante '(warning . haskell-hlint))))))
