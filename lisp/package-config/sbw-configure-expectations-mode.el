(require 'use-package)

(use-package expectations-mode
  :diminish expectations-mode
  :defer t

  :init
  (progn
    ;; Don't colorize the output if we're reading it in Emacs
    (setenv "EXPECTATIONS_COLORIZE" "false"))

  :config
  (progn
    ;; TODO This needs removing when expectations mode is updated
    (defun cider-load-current-buffer ()
      (interactive)
      (cider-load-buffer))))

(provide 'sbw-configure-expectations-mode)
