(use-package haskell-mode
  :init
  (progn
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))

    
    (require 'hindent)
    (add-hook 'haskell-mode-hook #'hindent-mode)

    
    ))

(provide 'sbw-configure-haskell-mode)

