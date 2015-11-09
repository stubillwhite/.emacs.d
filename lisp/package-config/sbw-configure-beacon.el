(require 'use-package)

(use-package beacon
  :diminish beacon-mode
  
  :init
  (progn
    (beacon-mode 1)
    
    (setq beacon-size 100)

    ;; Configure beacon-color after the theme has been loaded
    (add-hook 'after-init-hook (lambda () (setq beacon-color (face-background 'match nil t))))))

(provide 'sbw-configure-beacon)
