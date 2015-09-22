(require 'use-package)

(use-package clj-refactor
  :defer t
  
  :init
  (progn
    (add-hook 'cider-mode-hook 'clj-refactor-mode))

  :config
  (progn))

(provide 'sbw-clj-refactor)
