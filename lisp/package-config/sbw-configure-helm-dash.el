(require 'use-package)

(use-package helm-dash

  :init
  (progn
    (setq helm-dash-common-docsets '("Guava")))

  :config
  (progn)

  :bind
  ("<f2>" . helm-dash))

(provide 'sbw-configure-helm-dash)
