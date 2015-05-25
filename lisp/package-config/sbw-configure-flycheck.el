(require 'use-package)

(use-package flycheck
  :diminish flycheck-mode

  :init
  (progn
    (setq flycheck-html-tidy-executable "tidy5")
    
    (dolist (x '(html-mode-hook emacs-lisp-mode))
      (add-hook x (lambda () (flycheck-mode 1)))))

  :config
  (progn))

(provide 'sbw-configure-flycheck)
