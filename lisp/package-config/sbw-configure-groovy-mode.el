(require 'use-package)

(use-package groovy-mode
  :mode
  ("\\.\\(groovy\\|gradle\\)$" . groovy-mode)

  :config
  (progn
    (add-hook 'groovy-mode-hook 'sbw/set-java-style)))

(provide 'sbw-configure-groovy-mode)
