(require 'use-package)

(use-package evil
  :init
  (progn)
  
  :config
  (progn
    (defun sbw/evil-toggle-evil-mode ()
      (interactive)
      (if (bound-and-true-p evil-local-mode)
        (turn-off-evil-mode)
        (turn-on-evil-mode))))

  :bind
  ("<f6>" . sbw/evil-toggle-evil-mode))

(provide 'sbw-configure-evil)

