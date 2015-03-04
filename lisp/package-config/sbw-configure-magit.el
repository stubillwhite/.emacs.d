(require 'use-package)

(use-package magit
  :config
  (progn
    ;; Just highlight the current diff
    (setq-default ediff-highlight-all-diffs 'nil)

    (setq
      ediff-diff-options          "-w"                           ;; Not whitespace sensitive by default
      ediff-window-setup-function 'ediff-setup-windows-plain     ;; Single frame for Ediff mode-line
      ediff-split-window-function 'split-window-horizontally     ;; Vertical split
      )

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)


    ))

(provide 'sbw-configure-magit)
