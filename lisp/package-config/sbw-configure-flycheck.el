(require 'use-package)

(use-package flycheck
  :diminish flycheck-mode

  :init
  (progn  
    ;; Enable Flycheck for desired modes
    (dolist (x '(html-mode-hook emacs-lisp-mode markdown-mode shell-script-mode elm-mode))
      (add-hook x (lambda () (flycheck-mode 1))))
    
    ;; Disable annoying checkers
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc)))

  :config
  (progn
    ;; Prose checker
    (if (sbw/is-windows?)
        (flycheck-define-checker proselint
          "A linter for prose."
          :command        ("/cygdrive/c/Python34/python.exe" "-m" "proselint.command_line" (eval (cygwin-convert-file-name-to-windows (buffer-file-name))))
          :error-patterns ((warning line-start
                                    (optional (in "a-zA-Z") ":") (one-or-more (not (any ":"))) ":" line ":" column ": "
                                    (id (one-or-more (not (any " "))))
                                    (message (one-or-more not-newline) (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                                    line-end))
          :modes          (text-mode markdown-mode)))

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
    
    (add-to-list 'flycheck-checkers 'proselint)

    ;; HTML checker
    (setq flycheck-html-tidy-executable "tidy5")))

(provide 'sbw-configure-flycheck)
