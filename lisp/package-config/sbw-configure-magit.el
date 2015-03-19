(require 'use-package)

(use-package magit
  :defer t

  :init
  (progn)
  
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

    (defun sbw/magit-toggle-whitespace ()
      (interactive)
      (setq magit-diff-options
        (if (-contains? magit-diff-options "-w")
          (cons "-w" magit-diff-options)
          (-filter (lambda (x) (string= "-w" x)))))
      (magit-refresh)))

  :bind
  ("C-c g w" . sbw/magit-toggle-whitespace)
  ("C-c g s" . magit-status))

(provide 'sbw-configure-magit)
