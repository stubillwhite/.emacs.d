(use-package flycheck
  :diminish flycheck-mode

  :init
  (progn  
    ;; Enable Flycheck for desired modes
    (dolist (x '(html-mode-hook emacs-lisp-mode-hook markdown-mode-hook shell-script-mode-hook elm-mode-hook))
      (add-hook x (lambda () (flycheck-mode))))
    
    ;; Disable annoying checkers
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc)))

  :config
  (progn
    ;; Display errors if they exist, and close the buffer if they do not
    (add-hook 'flycheck-after-syntax-check-hook
              (lambda ()
                (if flycheck-current-errors
                    (flycheck-list-errors)
                  (when (get-buffer "*Flycheck errors*")
                    (switch-to-buffer "*Flycheck errors*")
                    (kill-buffer (current-buffer))
                    (delete-window)))))
  
    ;; Prose checker
    (if (sbw/is-linux?)
        (flycheck-define-checker proselint
          "A linter for prose."
          :command        ("python3" "-m" "proselint.command_line" source-inplace)
          :error-patterns ((warning line-start
                                    (file-name) ":" line ":" column ": "
                                    (id (one-or-more (not (any " "))))
                                    (message (one-or-more not-newline) (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                                    line-end))
          :modes          (text-mode markdown-mode)))

    (if (sbw/is-darwin?)
        (flycheck-define-checker proselint
          "A linter for prose."
          :command        ("proselint" source-inplace)
          :error-patterns ((warning line-start
                                    (file-name) ":" line ":" column ": "
                                    (id (one-or-more (not (any " "))))
                                    (message (one-or-more not-newline))
                                    line-end))
          :modes          (text-mode markdown-mode)))
    
    (add-to-list 'flycheck-checkers 'proselint)

    ;; HTML checker
    (setq flycheck-html-tidy-executable "tidy5")))

(provide 'sbw-configure-flycheck)
