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
  (set-frame-font "Monaco 10"))

(when (sbw/is-linux?)
  (set-frame-font "Monaco-10"))

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

;; Convert ANSI color codes in shell output
(require 'ansi-color)
(defun sbw/display-ansi-colors ()
  "Convert ANSI color codes in the current buffer to the appropriate colors."
  (interactive)
  (let ( (inhibit-read-only t) )
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Ediff configuration
(setq
 ediff-highlight-all-diffs   'nil                                        ;; Just highlight the current diff
 ediff-temp-file-prefix      (expand-file-name temporary-file-directory) ;; Temporary file location
 ediff-window-setup-function 'ediff-setup-windows-plain                  ;; Single frame for Ediff mode-line
 ediff-split-window-function 'split-window-horizontally                  ;; Vertical split
 )

;; Not whitespace sensitive by default
(setq-default ediff-ignore-similar-regions t)

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

;; Whitespace
;; TODO Move to a configure file
(require 'whitespace)
(setq whitespace-display-mappings '((space-mark   #x0020 [#x0020])
                                    (newline-mark #x000A [#x00B6 #x000A])
                                    (tab-mark     #x0009 [#x25BA #x0009])))
(setq whitespace-style
      (-remove (lambda (x) (-contains? '(lines lines-tail) x)) whitespace-style))

;; Uniquify
;; TODO Move to a configure file
(require 'uniquify)

(setq 
 uniquify-buffer-name-style  'forward
 uniquify-separator           "/"
 uniquify-after-kill-buffer-p t 
 uniquify-ignore-buffers-re   "^\\*")

;; CUA mode and shift-click to select
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy nil)
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) 
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

(provide 'sbw-cosmetics)
