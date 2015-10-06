(require 'use-package)

(use-package helm-ag
  :init
  (progn
    (custom-set-variables
     '(helm-ag-base-command "ack --nocolor --nogroup")))

  :config
  (progn))
