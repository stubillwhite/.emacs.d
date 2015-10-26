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
          (turn-on-evil-mode))))

    (defun sbw/evil-escape (prompt)
      (cond
       ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
       (t (kbd "C-g"))))

    (defun sbw/evil--configure-escape-alternative-key ()
      (define-key key-translation-map (kbd "C-l") 'sbw/evil-escape)
      (define-key evil-operator-state-map (kbd "C-l") 'keyboard-quit)
      (set-quit-char "C-l"))

    (add-hook 'evil-mode-hook 'sbw/evil--configure-escape-alternative-key))

  :bind
  ("C-c v" . sbw/evil-toggle-evil-mode)
  ("M-["   . sbw/evil-toggle-evil-mode))

