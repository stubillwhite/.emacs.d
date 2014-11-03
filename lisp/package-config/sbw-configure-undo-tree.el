(require 'use-package)

(use-package undo-tree
  :init
  (progn
    
    ;; Enable everywhere
    (global-undo-tree-mode t)

    ;; Easy alias
    (defalias 'redo 'undo-tree-redo)))

(provide 'sbw-setup-undo-tree)
