(require 'use-package)

(drag-stuff-mode 0)

(use-package drag-stuff
  :diminish drag-stuff-mode

  :init
  (progn
    (setq drag-stuff-except-modes '(org-mode))
    (drag-stuff-global-mode))

  :config
  (progn))

(provide 'sbw-configure-drag-stuff)
