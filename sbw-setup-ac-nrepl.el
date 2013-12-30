(require 'ac-nrepl)

;; Enable autocomplete in Cider mode
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook      'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Add to tab completion functions
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook    'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook         'set-auto-complete-as-completion-at-point-function)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(provide 'sbw-setup-ac-nrepl)
