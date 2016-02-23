(require 'use-package)

(use-package helm-dash

  :init
  (progn)

  :config
  (progn
    (helm-dash-activate-docset "Guava"))

  :bind
  ("<f2>" . helm-dash))

(provide 'sbw-configure-helm-dash)
