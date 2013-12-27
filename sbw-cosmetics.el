;; Color theme, don't warn about executing code
(load-theme 'sbw-dark t)

;; Maximise the screen area by stripping off menu and toolbars
(tool-bar-mode -1)
(menu-bar-mode -1) 

;; Indentation
(setq lisp-indent-offset 2)                      ;; Default indent for Lisps
(setq-default indent-tabs-mode nil)              ;; Indent spaces not tabs
(setq-default truncate-lines t)                  ;; Truncate lines don't wrap

;; Font
(set-default-font "-outline-Lucida Console-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1")

;; Favor horizontal splits over vertical splits
(setq 
  split-width-threshold  nil
  split-height-threshold 0)

;; No scrollbars
(scroll-bar-mode -1)

;; Smooth scrolling
(setq scroll-conservatively 10000)

;; Show matching parenthesis
(show-paren-mode 1)

;; Start with a maximised window
(defun sbw-maximise-window ()
  (interactive)
  (w32-send-sys-command #xf030))

(setq 
  term-setup-hook   'sbw-maximise-window
  window-setup-hook 'sbw-maximise-window)

(provide 'sbw-cosmetics)
