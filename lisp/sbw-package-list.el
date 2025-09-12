(setq sbw/package-list '(
                         ;; Package management
                         (:name use-package) ;; Easy package use

                         ;; Elisp
                         (:name dash)            ;; Modern list API
                         (:name dash-functional) ;; Additional functions for dash
                         (:name f)               ;; Modern file API
                         (:name ht)              ;; The missing hash table library for Emacs
                         (:name s)               ;; Modern string API
                         (:name seq)             ;; Unified abstractions for sequences
                         (:name nameless)        ;; Less is more. Hide package namespace in your emacs-lisp code

                         ;; Completion
                         (:name helm)            ;; Incremental narrowing framework
                         (:name helm-projectile) ;; Helm integration for projectile
                         (:name helm-swoop)      ;; Efficiently skipping between matches
                         (:name helm-rg)         ;; Ripgrep with Helm interface
                         (:name helm-descbinds)  ;; A helm frontend for describe-bindings
                         (:name helm-flycheck)   ;; Show flycheck errors with Helm
                         (:name helm-flyspell)   ;; Helm extension for correcting words with Flyspell
                         (:name helm-dash)       ;; Browse Dash docsets inside Emacs

                         ;; Auto-complete
                         (:name company)   ;; Auto-completion
                         (:name yasnippet) ;; Yet another snippet extension for Emacs

                         ;; Interface
                         (:name powerline)           ;; Emacs version of the Vim powerline
                         (:name diminish)            ;; Abbreviate minor mode indicators
                         (:name projectile)          ;; Project interaction library for Emacs
                         (:name avy)                 ;; Jump to things in Emacs tree-style
                         (:name ace-window)          ;; Quickly switch windows
                         (:name expand-region)       ;; Expand region by semantic units
                         (:name drag-stuff)          ;; Drag stuff around in Emacs
                         (:name evil-nerd-commenter) ;; Efficient language-independent commenting
                         (:name key-chord)           ;; Map pairs of simultaneously pressed keys to commands
                         (:name hydra)               ;; Make Emacs bindings that stick around
                         (:name multiple-cursors)    ;; An experiment in adding multiple cursors to emacs
                         (:name which-key)           ;; Display available key bindings
                         (:name undo-fu)             ;; Simple, stable linear undo with redo for Emacs.
                         (:name zoom-frm)            ;; Commands to zoom frame font size
                         (:name beacon)              ;; A light that follows your cursor around so you don't lose it!

                         ;; Parenthesis management
                         (:name smartparens)  ;; Improved paredit

                         ;; Static checking
                         (:name flyspell)              ;; Spell checking
                         (:name flyspell-correct)      ;; Correcting words with flyspell via custom interface
                         (:name flyspell-correct-helm) ;; Correcting words with flyspell via custom interface
                         (:name flycheck)              ;; Syntax checking
                         ;; (:name flymake-vale)          ;; Flymake support for Vale

                         ;; org-mode
                         (:name org)              ;; Your life in plain text
                         (:name org-contrib)      ;; Additions to org-mode
                         (:name ob-async)         ;; Asynchronous src_block execution for org-babel
                         (:name orgtbl-aggregate) ;; Aggregation for org-mode tables
                         (:name htmlize)          ;; html org-mode reports

                         ;; Git
                         (:name magit) ;; Control Git from Emacs

                         ;; Regex
                         (:name visual-regexp)          ;; A regexp/replace command for Emacs with interactive visual feedback
                         (:name visual-regexp-steroids) ;; Extends visual-regexp to support other regexp engines

                         ;; Languages

                         ;; Clojure
                         (:name clojure-mode) ;; Clojure mode
                         (:name cider)        ;; REPL support
                         (:name clj-refactor) ;; Clojure refactoring functions
                         (:name cljr-helm)    ;; Helm wrapper for clj-refactor

                         ;; Markdown
                         (:name markdown-mode) ;; Markdown mode
                         (:name markdown-toc)  ;; Generate a table of contents in a markdown file

                         ;; Text
                         (:name mw-thesaurus) ;; Merriam-Webster Thesaurus

                         ;; YAML
                         (:name yaml-mode)         ;; Emacs major mode for editing YAML files
                         (:name flycheck-yamllint) ;; YAML checker
                         ))

(setq sbw/personal-package-list
      '(
        sbw-hash-tables
        sbw-hash-tables-test

        sbw-multimethods
        sbw-multimethods-test

        sbw-value-eq
        sbw-value-eq-test

        sbw-org-utils
        sbw-org-utils-test

        sbw-cosmetics-code-style
        sbw-bindings
        sbw-utils
        sbw-utils-test
        sbw-misc
        sbw-org-config

        sbw-org-review
        sbw-org-review-test

        sbw-org-tech-radar
        sbw-org-tech-radar-test

        sbw-menu
        sbw-countdown

        sbw-time
        sbw-time-test))

(provide 'sbw-package-list)
