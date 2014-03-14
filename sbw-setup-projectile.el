(require 'projectile)

(setq projectile-enable-caching t)

(projectile-global-mode)

(global-set-key (kbd "C-c C-f") 'projectile-find-file)

(provide 'sbw-setup-projectile)
