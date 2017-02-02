(require 'use-package)

(use-package avy
  :defer t

  :init
  (progn)

  :config
  (progn
    (setq
      avy-background t  ;; Darken text when jumping
      ))

  :bind
  ("C-c j" . avy-goto-char)
  ("C-c J" . avy-pop-mark))

(provide 'sbw-configure-avy)
