;; Use ParEdit mode in Clojure and the REPL
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'nrepl-mode-hook   'paredit-mode)

;; Enable ElDoc in Clojure buffers
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; Prevent the error buffer popping up while working in the REPL
(setq nrepl-popup-stacktraces nil)

;; Make c-c c-z switch to *nrepl*
(add-to-list 'same-window-buffer-names "*nrepl*") 

;; Enable CamelCase support for motion commands
(add-hook 'nrepl-mode-hook 'subword-mode)

;; Auto-complete
(add-hook 'nrepl-mode-hook             'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
                 '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook     'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook             'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Keybindings
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\M-[" 'paredit-wrap-square)
             (define-key clojure-mode-map "\M-{" 'paredit-wrap-curly)
           ))

(setq nrepl-port "4555")

(provide 'sbw-clojure)
