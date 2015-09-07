(require 'use-package)

(use-package flycheck
  :diminish flycheck-mode

  :init
  (progn
    (setq flycheck-html-tidy-executable "tidy5")

    ;; Enable Flycheck for desired modes
    (dolist (x '(html-mode-hook emacs-lisp-mode))
      (add-hook x (lambda () (flycheck-mode 1))))

    ;; Disable annoying checkers
    (setq-default flycheck-disabled-checkers
      '(emacs-lisp-checkdoc)))

  :config
  (progn))

(provide 'sbw-configure-flycheck)
