(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(ac-flyspell-workaround)

(global-auto-complete-mode t)
(setq ac-auto-show-menu                         t
      ac-dwim                                   t
      ac-use-menu-map                           t
      ac-quick-help-delay                       1
      ac-quick-help-height                      60
      ac-disable-inline                         t
      ac-show-menu-immediately-on-auto-complete t
      ac-auto-start                             2
      ac-candidate-menu-min                     0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))

(dolist (mode '(org-mode text-mode lisp-mode))
  (add-to-list 'ac-modes mode))

;; Key triggers
(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map "\t"          'ac-complete)
(define-key ac-completing-map (kbd "M-RET") 'ac-help)
(define-key ac-completing-map "\r"          'nil)

(provide 'sbw-setup-auto-complete)
