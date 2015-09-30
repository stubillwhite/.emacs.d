(require 'use-package)

(use-package magit
  :defer t

  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"))
  
  :config
  (progn
    ;; Point to git.exe in Windows systems for a more responsive Magit
    (if (eq system-type 'cygwin)
      (setq magit-git-executable "/usr/bin/git.exe"))   
    )

  :bind
  ("C-c g s" . magit-status))

(provide 'sbw-configure-magit)

