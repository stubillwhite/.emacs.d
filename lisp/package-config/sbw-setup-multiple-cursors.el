(require 'use-package)

(use-package multiple-cursors
  :init
  (progn
    (global-set-key (kbd "C->")     'mc/mark-next-like-this)
    (global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(provide 'sbw-setup-multiple-cursors)
