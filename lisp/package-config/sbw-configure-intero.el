(require 'use-package)

(use-package intero
  :config
  (progn
    (add-hook 'haskell-mode-hook 'intero-mode)
    
    (setq flycheck-check-syntax-automatically '(save new-line))
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

    (defun sbw/intero--ensure-vertical-split (orig-fun &rest args)
      (let* ( (orig-width  split-width-threshold)
              (orig-height split-height-threshold) )
        (save-some-buffers t)
        (setq split-width-threshold  0
              split-height-threshold nil)    
        (apply orig-fun args)
        (setq split-width-threshold  orig-width
              split-height-threshold orig-height)))
    (advice-add 'intero-repl :around #'sbw/intero--ensure-vertical-split)))

(provide 'sbw-configure-intero)
