(require 'use-package)

(use-package scala-mode2
  :init
  (progn
    (setq scala-indent:align-parameters t ;; Indent parameters under first
          scala-indent:align-forms      t ;; Indent forms under assignment
          ))

  :config
  (progn))

(provide 'sbw-configure-scala-mode2)
