(require 'use-package)

(use-package clj-refactor
  :defer t
  :diminish clj-refactor-mode
  
  :init
  (progn
    (add-hook 'cider-mode-hook 'clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c C-m"))

  :config
  (progn))

(provide 'sbw-clj-refactor)
