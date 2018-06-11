(use-package projectile
  :diminish projectile-mode
  
  :init
  (progn
    (projectile-global-mode)
    
    ;; Ensure that Windows is picking up the right version of grep
    (when (sbw/is-windows?)
      (setq exec-path (append '("/usr/bin") exec-path))))

  :config
  (progn))

(provide 'sbw-configure-projectile)
