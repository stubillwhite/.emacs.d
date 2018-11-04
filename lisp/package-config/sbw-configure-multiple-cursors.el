(use-package multiple-cursors
  :defer t

  :init
  (progn)
  
  :config
  (progn)

  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-." . mc/unmark-next-like-this)
  ("C-," . mc/unmark-previous-like-this)
  ("M-n" . mc/skip-to-next-like-this)
  )

(provide 'sbw-configure-mutiple-cursors)
