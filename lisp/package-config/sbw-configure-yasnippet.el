(require 'use-package)

(use-package yasnippet
  :defer t

  :init
  (progn) 

  :config
  (progn
    (add-to-list 'yas/root-directory "~/.emacs.d/snippets")
    (yas/initialize)

    (unbind-key "TAB"))
  
  :bind
  ("<backtab>" . yas/expand))

(provide 'sbw-configure-yasnippet)
