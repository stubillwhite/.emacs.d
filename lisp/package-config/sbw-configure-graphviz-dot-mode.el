(require 'use-package)

(use-package graphviz-dot-mode
  :defer t

  :init
  (progn)

  :config
  (progn
    (setq
      graphviz-dot-dot-program  "dot.exe"
      graphviz-dot-view-command "dotty.exe %s"))

  :bind
  ( ("C-c d v" . graphviz-dot-view) ))

(provide 'sbw-configure-graphviz-dot-mode)
