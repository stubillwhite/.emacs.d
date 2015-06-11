(require 'use-package)

(use-package multiple-cursors
  :defer t

  :init
  (progn)
  
  :config
  (progn)

  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(provide 'sbw-configure-mutiple-cursors)
