(setq sbw/package-list '(
        ;; Package management
        (:name use-package) ;; Easy package use

        ;; Elisp
        (:name dash)            ;; Modern list API
        (:name dash-functional) ;; Additional functions for dash
        (:name f)               ;; Modern file API
        (:name s)               ;; Modern string API
        (:name seq)             ;; Unified abstractions for sequences

        ;; Completion
        (:name ivy)     ;; Completion framework
        (:name counsel) ;; Ivy enhancements for common commands
        (:name swiper)  ;; Ivy alternative for search

        ;; Interface
        (:name diminish)           ;; Abbreviate minor mode indicators
        (:name projectile)         ;; Project interaction library for Emacs
        (:name counsel-projectile) ;; Ivy UI for Projectile
        (:name powerline)          ;; Emacs version of the Vim powerline
        (:name expand-region)      ;; Expand region by semantic units
        (:name hydra)              ;; Make Emacs bindings that stick around

        ;; Parenthesis management
        (:name lispy) ;; Short and sweet Lisp editing
        
        ;; Usability
        (:name evil)        ;; Welcome home
        (:name evil-leader) ;; Add <leader> shortcuts to Evil, the extensible vim emulation layer
        (:name key-chord)   ;; Map pairs of simultaneously pressed keys to commands

        ;; Static checking
        (:name flyspell)             ;; Spell checking
        (:name flyspell-correct)     ;; Correcting words with flyspell via custom interface
        (:name flyspell-correct-ivy) ;; Correcting words with flyspell via custom interface
        (:name flycheck)             ;; Syntax checking

        ;; org-mode
        (:name org)      ;; Your life in plain text
        (:name htmlize)  ;; html org-mode reports
        (:name org-gcal) ;; Org sync with Google Calendar

        ;; Git
        ;; (:name magit) ;; Control Git from Emacs
 
        ;; Slack
        (:name slack)   ;; Slack for Emacs
        (:name emojify) ;; Emoji for Slack

        ;; Languages

        ;; Clojure
        (:name clojure-mode) ;; Clojure mode
        (:name cider)        ;; REPL support
        ;; (:name smartparens)  ;; Improved paredit
        (:name clj-refactor) ;; Clojure refactoring functions
        ;; (:name cljr-helm)    ;; Helm wrapper for clj-refactor

        ;; ;; Elm
        ;; elm-mode     ;; Elm mode for emacs
        ;; flycheck-elm ;; Flycheck for Elm
     
        ;; Haskell
        (:name haskell-mode) ;; Haskell mode
        (:name intero)       ;; Interactive Haskell development

        ;; ;; ;; JSON
        ;; ;; json-mode ;; Major mode for editing JSON files

        ;; ;; Markdown
        ;; markdown-mode ;; Markdown mode
        ))

(setq sbw/personal-package-list
      '(
        sbw-cosmetics-code-style
        sbw-bindings
        sbw-utils
        ))

(provide 'sbw-package-list)
