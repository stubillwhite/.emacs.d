(require 'use-package)

(use-package beacon
  :diminish beacon-mode
  
  :init
  (progn
    (beacon-mode 1)
    (setq
     beacon-color (face-background 'match nil t)
     beacon-size  150)))

(provide 'sbw-configure-beacon)
