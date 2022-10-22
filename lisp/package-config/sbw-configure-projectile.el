(use-package projectile
  :diminish projectile-mode
  
  :init
  (progn
    (projectile-mode t)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

    ;; Ensure that Windows is picking up the right version of grep
    (when (sbw/is-windows?)
      (setq exec-path (append '("/usr/bin") exec-path)))

    (setq
     projectile-indexing-method 'hybrid  ;; Slower processing, but respect .projectile
     ))

  :config
  (progn))

(provide 'sbw-configure-projectile)
