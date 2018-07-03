(require 'use-package)

(use-package evil
  :init
  (progn)
  
  :config
  (progn
    (defvar sbw/evil--good-cursor-type nil
      "The value of the cursor-type before activating Evil mode.")
    
    (defun sbw/evil-toggle-global-evil-mode ()
      (interactive)
      (if (bound-and-true-p evil-mode)
          (progn
            (evil-mode -1)
            (setq cursor-type sbw/evil--good-cursor-type))
        (progn
          (setq sbw/evil--good-cursor-type cursor-type)
          (evil-mode))))

    (sbw/evil-toggle-global-evil-mode)

    (evil-define-key nil evil-normal-state-map
                     "z=" 'flyspell-correct-at-point)))
