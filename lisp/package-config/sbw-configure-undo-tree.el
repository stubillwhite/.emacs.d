(require 'use-package)

(use-package undo-tree
  :diminish undo-tree-mode
  
  :init
  (progn
    ;; Enable everywhere
    (global-undo-tree-mode t)

    ;; Persistent undo between sessions, use the same directory as other backups
    (setq
      undo-tree-auto-save-history       t
      undo-tree-history-directory-alist (quote (("." . "~/.emacs-undo-tree/"))))
    
    ;; Easy alias
    (defalias 'redo 'undo-tree-redo))

  :bind
  ("C-z"   . undo-tree-undo)
  ("C-c u" . undo-tree-visualize))

(provide 'sbw-configure-undo-tree)
