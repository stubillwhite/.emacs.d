(require 'use-package)

(use-package evil-nerd-commenter
  :config
  (progn)

  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(provide 'sbw-configure-evil-nerd-commenter)
