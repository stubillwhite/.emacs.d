(require 'use-package)

(use-package dash
  :config
  (progn
    ;; Syntax highlighting for dash functions
    (dash-enable-font-lock)))

(provide 'sbw-configure-dash)
