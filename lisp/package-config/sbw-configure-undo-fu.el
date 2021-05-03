(use-package undo-fu
  :init
  (progn
    (defalias 'redo 'undo-fu-only-redo))

  :bind
  ("C-z"   . 'undo-fu-only-undo)
  ("C-S-z" . 'undo-fu-only-redo))

(provide 'sbw-configure-undo-fu)
