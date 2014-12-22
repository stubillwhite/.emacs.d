(require 'use-package)

(use-package undo-tree
  :init
  (progn
    ;; Enable everywhere
    (global-undo-tree-mode t)

    ;; Persistent undo between sessions, use the same directory as other backups
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist backup-directory-alist)

    ;; Easy alias
    (defalias 'redo 'undo-tree-redo)))

(provide 'sbw-configure-undo-tree)
