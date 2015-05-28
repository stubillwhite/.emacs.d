(require 'use-package)

(use-package evil
  :init
  (progn)
  
  :config
  (progn

    (defvar sbw/evil--good-cursor-type nil
      "The value of the cursor-type before activating Evil mode.")
    
    (defun sbw/evil-toggle-evil-mode ()
      (interactive)
      (if (bound-and-true-p evil-local-mode)
        (progn
          (turn-off-evil-mode)
          (setq cursor-type sbw/evil--good-cursor-type))
        (progn
          (setq sbw/evil--good-cursor-type cursor-type)
          (turn-on-evil-mode)))))

  :bind
  ("C-c v" . sbw/evil-toggle-evil-mode)
  ("M-["   . sbw/evil-toggle-evil-mode)
  ("M-q"   . evil-visual-block)
  )

(provide 'sbw-configure-evil)
