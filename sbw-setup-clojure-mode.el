;; Use ParEdit mode in Clojure and the REPL
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; Keybindings
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\M-[" 'paredit-wrap-square)
             (define-key clojure-mode-map "\M-{" 'paredit-wrap-curly)
           ))

(provide 'sbw-setup-clojure-mode)
