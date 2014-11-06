(require 'use-package)

;; See https://github.com/bbatsov/projectile

(use-package projectile
  :diminish projectile-mode
  
  :init
  (progn
    (projectile-global-mode)

    (require 'helm-projectile)
    (helm-projectile-on))

  :config
  (progn))

(provide 'sbw-configure-projectile)
