;; Color theme, don't warn about executing code
(load-theme 'sbw-dark-muted t)

;; Maximise the screen area by stripping off menu, toolbars, and scrollbars
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format sbw/frame-title-format)

;; Indentation
(setq-default indent-tabs-mode nil) ;; Indent spaces not tabs
(setq-default truncate-lines t)     ;; Truncate lines don't wrap

;; Font
(when (sbw/is-windows?)
  ;; (set-frame-font "-outline-Lucida Console-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1")
  ;; (set-frame-font "Lucida Console-10")
  ;; (set-frame-font "Inconsolata-dz-10")
  (set-frame-font "Monaco 10")
  )

(when (sbw/is-linux?)
  ;; (set-frame-font "DejaVu Sans Mono-10")
  ;; (set-frame-font "Ubuntu Mono-12")
  ;; (set-frame-font "Droid Sans Mono-10")
  ;; (set-frame-font "Free Mono-10")
  ;; (set-frame-font "Liberation Mono-10")
  ;; (set-frame-font "Nimbus Mono L-10")
  (set-frame-font "Monaco-10")
  )

(when (sbw/is-darwin?)
  (set-frame-font "Monaco-12"))

;; Horizontal non-blinking cursor
(setq-default cursor-type 'hbar)
(blink-cursor-mode 0)

;; Prettify symbols everywhere
(global-prettify-symbols-mode 1)

;; Favor horizontal splits over vertical splits
(setq 
  split-width-threshold  nil
  split-height-threshold 0)

;; Smooth scrolling
(setq scroll-conservatively 10000)

;; Show matching parenthesis
(show-paren-mode 1)

;; Start with a maximised window
(setq window-setup-hook 'toggle-frame-maximized)

;; Hide DOS EOL characters
(defun sbw/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; Ediff configuration
(setq
  ediff-highlight-all-diffs   'nil                                            ;; Just highlight the current diff
  ediff-temp-file-prefix      (expand-file-name temporary-file-directory)     ;; Temporary file location
  ediff-diff-options          "-w"                                            ;; Not whitespace sensitive by default
  ediff-window-setup-function 'ediff-setup-windows-plain                      ;; Single frame for Ediff mode-line
  ediff-split-window-function 'split-window-horizontally                      ;; Vertical split
  )

(defun sbw/cosmetics-save-window-config ()
  "Store the current window configuration."
  (setq sbw/cosmetics--saved-window-config (current-window-configuration)))

(defun sbw/cosmetics-restore-window-config ()
  "Restore the last saved window configuration."
  (when (boundp 'sbw/cosmetics--saved-window-config)
    (set-window-configuration sbw/cosmetics--saved-window-config)))

(add-hook 'ediff-before-setup-hook 'sbw/cosmetics-save-window-config)
(add-hook 'ediff-quit-hook         'sbw/cosmetics-restore-window-config)
(add-hook 'ediff-suspend-hook      'sbw/cosmetics-restore-window-config)

(provide 'sbw-cosmetics)
