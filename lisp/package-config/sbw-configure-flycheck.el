(use-package flycheck
  :diminish flycheck-mode

  ;; TODO: Autoformatting of source code
  ;; TODO: Insert or complete
  
  :init
  (progn
    ;; Enable Flycheck for desired modes
    (dolist (x '(html-mode-hook emacs-lisp-mode-hook markdown-mode-hook shell-script-mode-hook elm-mode-hook yaml-mode-hook))
      (add-hook x (lambda () (flycheck-mode))))

    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

    ;; Disable annoying checkers
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc)))

  :config
  (progn
    ;; Display errors if they exist, and close the buffer if they do not
    (setq sbw/flycheck-auto-display-errors nil)

    (defun sbw/flycheck--display-or-hide-errors ()
      (if (and flycheck-current-errors sbw/flycheck-auto-display-errors)
          (flycheck-list-errors)
        (when (get-buffer "*Flycheck errors*")
          (switch-to-buffer "*Flycheck errors*")
          (kill-buffer (current-buffer))
          (delete-window))))

    (add-hook 'flycheck-after-syntax-check-hook
              'sbw/flycheck--display-or-hide-errors)

    (defun sbw/flycheck-toggle-auto-display-errors ()
      (interactive)
      (if sbw/flycheck-auto-display-errors
          (progn
            (setq sbw/flycheck-auto-display-errors nil)
            (message "Disabled auto display of Flycheck errors"))
        (progn
          (setq sbw/flycheck-auto-display-errors t)
          (message "Enabled auto display of Flycheck errors")))
      (sbw/flycheck--display-or-hide-errors))

    ;; Prose checker
   
    
    ;;(add-to-list 'flycheck-checkers 'proselint)

    ;; HTML checker
    (setq flycheck-html-tidy-executable "tidy5")))

(provide 'sbw-configure-flycheck)
