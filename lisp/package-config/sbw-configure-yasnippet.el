(require 'use-package)

(use-package yasnippet
  :defer t

  :init
  (progn) 

  :config
  (progn
    (add-to-list 'yas/root-directory "~/.emacs.d/snippets")
    (yas/initialize)

    (bind-key "<backtab>" 'yas/expand)
    (unbind-key "TAB")
    )
  )

(provide 'sbw-configure-yasnippet)
