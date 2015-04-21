(require 'use-package)

(use-package smartparens
  :diminish smartparens-mode
  
  :init
  (progn
    ;; Global mode
    (require 'smartparens-config)
    (smartparens-global-mode t)

    ;; Highlight matching pairs
    (show-smartparens-global-mode t)

    ;; Strict mode
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
    (add-hook 'clojure-mode-hook    'smartparens-strict-mode)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
    )

  :config
  (progn
    ;; Keybindings
                                        ;(sp-local-pair 'clojure-mode "(" nil :bind "C-(")
                                        ;(sp-local-pair 'emacs-lisp-mode "(" nil :bind "C-(")

                                        ; Should work; is on the smartparens wiki
                                        ; https://github.com/Fuco1/smartparens/wiki/Pair-management
                                        ;(sp-pair "(" ")" :wrap "C-(")
                                        ;(sp-pair "(" ")" :wrap "C-(")

    ;; (sp-with-modes sp--lisp-modes
    ;;   (sp-local-pair "'" nil :actions nil)
    ;;   (sp-local-pair "`" "'" :when '(sp-in-string-p)))
    ;; 
    ;; (sp-with-modes 'clojure-mode
    ;;   (sp-local-pair "(" ")" :actions '(insert wrap)))


    (define-key sp-keymap (kbd "C-]")          'sp-select-next-thing-exchange)
                                        ;(define-key sp-keymap (kbd "C-(") "\C-](")
                                        ;(define-key sp-keymap (kbd "C-[") "\C-][")
                                        ;(define-key sp-keymap (kbd "C-{") "\C-]{")

    (define-key sp-keymap (kbd "<delete>")          'sp-delete-char)
    (define-key sp-keymap (kbd "C-<right>")         'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>")          'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-S-<right>")       'sp-backward-barf-sexp)
    (define-key sp-keymap (kbd "C-S-<left>")        'sp-backward-slurp-sexp)
                                        ;(define-key sp-keymap (kbd "M-s")               'sp-unwrap-sexp)
                                        ;(define-key sp-keymap (kbd "M-S-s")             'sp-raise-sexp)
    (define-key sp-keymap (kbd "M-p")               'sp-split-sexp)
    (define-key sp-keymap (kbd "M-S-p")             'sp-join-sexp)
    ;;(define-key sp-keymap (kbd "M-t")               'sp-transpose-sexp)
    ;;(define-key sp-keymap (kbd "M-S-<left>")        'sp-backward-sexp)
    ;;(define-key sp-keymap (kbd "M-S-<right>")       'sp-forward-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "C-M-f")            'sp-forward-sexp)
    ;;(define-key sp-keymap (kbd "C-M-b")            'sp-backward-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "C-M-d")            'sp-down-sexp)
    ;;(define-key sp-keymap (kbd "C-M-a")            'sp-backward-down-sexp)
    ;;(define-key sp-keymap (kbd "C-S-a")            'sp-beginning-of-sexp)
    ;;(define-key sp-keymap (kbd "C-S-d")            'sp-end-of-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "C-M-e")            'sp-up-sexp)
    ;;(define-key emacs-lisp-mode-map (kbd ")")      'sp-up-sexp)
    ;;(define-key sp-keymap (kbd "C-M-u")            'sp-backward-up-sexp)
    ;;(define-key sp-keymap (kbd "C-M-t")            'sp-transpose-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "C-M-n")            'sp-next-sexp)
    ;;(define-key sp-keymap (kbd "C-M-p")            'sp-previous-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "C-M-k")            'sp-kill-sexp)
    ;;(define-key sp-keymap (kbd "C-M-w")            'sp-copy-sexp)
    ;;
    (define-key sp-keymap (kbd "M-<delete>")       'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>")    'sp-backward-unwrap-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "C-<right>")        'sp-forward-slurp-sexp)
    ;;(define-key sp-keymap (kbd "C-<left>")         'sp-forward-barf-sexp)
    ;;(define-key sp-keymap (kbd "C-M-<left>")       'sp-backward-slurp-sexp)
    ;;(define-key sp-keymap (kbd "C-M-<right>")      'sp-backward-barf-sexp)
    ;;
    ;;(define-key sp-keymap (kbd "M-D")              'sp-splice-sexp)
    ;;(define-key sp-keymap (kbd "C-M-<delete>")     'sp-splice-sexp-killing-forward)
    ;;(define-key sp-keymap (kbd "C-M-<backspace>")  'sp-splice-sexp-killing-backward)
    ;;(define-key sp-keymap (kbd "C-S-<backspace>")  'sp-splice-sexp-killing-around)
    ;;
    ;;(define-key sp-keymap (kbd "C-]")              'sp-select-next-thing-exchange)
    ;;(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    ;;(define-key sp-keymap (kbd "C-M-]")            'sp-select-next-thing)
    ;;
    ;;(define-key sp-keymap (kbd "M-F")              'sp-forward-symbol)
    ;;(define-key sp-keymap (kbd "M-B")              'sp-backward-symbol)

    ;; (define-key sp-keymap (kbd "H-t")              'sp-prefix-tag-object)
    ;; (define-key sp-keymap (kbd "H-p")              'sp-prefix-pair-object)
    ;; (define-key sp-keymap (kbd "H-s c")            'sp-convolute-sexp)
    ;; (define-key sp-keymap (kbd "H-s a")            'sp-absorb-sexp)
    ;; (define-key sp-keymap (kbd "H-s e")            'sp-emit-sexp)
    ;; (define-key sp-keymap (kbd "H-s p")            'sp-add-to-previous-sexp)
    ;; (define-key sp-keymap (kbd "H-s n")            'sp-add-to-next-sexp)
    ;; (define-key sp-keymap (kbd "H-s j")            'sp-join-sexp)
    ;; (define-key sp-keymap (kbd "H-s s")            'sp-split-sexp)

                                        ;(sp-with-modes '( lisp-mode 
                                        ;clojure-mode)
                                        ;(sp-local-pair "(" nil :bind "M-(")
                                        ;(sp-local-pair "[" nil :bind "M-[")
                                        ;(sp-local-pair "{" nil :bind "M-{")
                                        ;(sp-local-pair "\"" nil :bind "M-\""))

                                        ;(sp-with-modes '(emacs-lisp-mode clojure-mode common-lisp-mode)
                                        ;(sp-local-pair "(" nil :bind "M-(")
                                        ;(sp-local-pair "'" nil :actions nil)
                                        ;(sp-local-pair "`" "'" :when '(sp-in-string-p)))

                                        ;(sp-with-modes sp--lisp-modes
                                        ;(sp-local-pair "(" nil :bind "C-("))
    
    ))



(provide 'sbw-configure-smartparens)
