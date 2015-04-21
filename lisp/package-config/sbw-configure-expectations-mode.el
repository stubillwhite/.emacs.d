(require 'use-package)

(use-package expectations-mode
  :defer t

  :init
  (progn)

  :config
  (progn
    ;; TODO This needs removing when expectations mode is updated
    (defun cider-load-current-buffer ()
      (interactive)
      (cider-load-buffer))))

(provide 'sbw-configure-expectations-mode)
