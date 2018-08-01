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

    (setq evil-want-fine-undo t)

    (evil-define-key nil evil-normal-state-map "z=" 'flyspell-correct-at-point)

    ;; Don't use evil-emacs-state

     (defun sbw/evil--revert-evil-emacs-state ()
       (evil-normal-state)
       (remove-hook 'post-command-hook 'sbw/evil--revert-evil-emacs-state))
     (add-hook 'evil-emacs-state-entry-hook (lambda () (add-hook 'post-command-hook 'sbw/evil--revert-evil-emacs-state)))


  (defun toggle-evilmode ()
    (interactive)
    (if (bound-and-true-p evil-local-mode)
        (progn
          (evil-local-mode (or -1 1))
          (undo-tree-mode (or -1 1))
          (set-variable 'cursor-type 'bar))
      (progn
        (evil-local-mode (or 1 1))
        (set-variable 'cursor-type 'box))))

    ;; (eval-after-load "evil-maps"
    ;;   '(progn
    ;;      (global-unset-key (kbd "C-z"))))
    
    (dolist (mode '(git-rebase-mode))
      (add-to-list 'evil-emacs-state-modes mode))
    ))
