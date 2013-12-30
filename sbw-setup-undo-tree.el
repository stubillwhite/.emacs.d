(require 'undo-tree)

(global-undo-tree-mode t)

(defalias 'redo 'undo-tree-redo)

(provide 'sbw-setup-undo-tree)
