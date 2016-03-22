(require 'use-package)

(use-package flycheck
  :diminish flycheck-mode

  :init
  (progn
    ;; Prose checker
    (flycheck-define-checker proselint
      "A linter for prose."
      :command        ("python3" "-m" "proselint.command_line" source-inplace)
      :error-patterns ((warning line-start (file-name) ":" line ":" column ": "
                                (id (one-or-more (not (any " "))))
                                (message (one-or-more not-newline)
                                         (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                                line-end))
      :modes          (text-mode markdown-mode gfm-mode))

    (add-to-list 'flycheck-checkers 'proselint)

    ;; HTML checker
    (setq flycheck-html-tidy-executable "tidy5")
    
    ;; Enable Flycheck for desired modes
    (dolist (x '(html-mode-hook emacs-lisp-mode markdown-mode))
      (add-hook x (lambda () (flycheck-mode 1))))

    ;; Disable annoying checkers
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc)))

  :config
  (progn))

(provide 'sbw-configure-flycheck)
