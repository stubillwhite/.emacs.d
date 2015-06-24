(require 'use-package)

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode
  
  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"))
  
  :config
  (progn
    
    (setq-default
      ediff-highlight-all-diffs 'nil                                        ;; Just highlight the current diff
      ediff-temp-file-prefix    (expand-file-name temporary-file-directory) ;; Temporary file location
      )

    (setq
      ediff-diff-options          "-w"                           ;; Not whitespace sensitive by default
      ediff-window-setup-function 'ediff-setup-windows-plain     ;; Single frame for Ediff mode-line
      ediff-split-window-function 'split-window-horizontally     ;; Vertical split
      )

    ;; (if (eq system-type 'cygwin)
    ;;   (progn
    ;;     (setq exec-path (add-to-list 'exec-path "/usr/bin"))
    ;;     (setenv "PATH" (concat "C:\\cygwin32\\bin;" (getenv "PATH")))))
    (setq magit-git-executable "/usr/bin/git.exe")
    )

  :bind
  ("C-c g s" . magit-status))

(provide 'sbw-configure-magit)

