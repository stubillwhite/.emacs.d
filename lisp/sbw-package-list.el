;; (defun my/helm-buffers-list ()
;;   ;; https://stackoverflow.com/questions/11403862/how-to-have-emacs-helm-list-offer-files-in-current-directory-as-options
;;   (interactive)
  
;;   (unless helm-source-buffers-list
;;     (setq helm-source-buffers-list
;;           (helm-make-source "Buffers" 'helm-source-buffers)))

;;   (helm-other-buffer '(helm-source-buffers-list
;;                        helm-source-files-in-current-dir
;;                        helm-source-bookmarks
;;                        helm-source-recentf
;;                        helm-source-bindir
;;                        helm-source-projectile-files-list)
;;                    "* my/helm-find-files *"))

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
                         (:name helm-ag)         ;; The silver searcher with Helm interface
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
                         
                         ;; Usability
                         (:name key-chord)        ;; Map pairs of simultaneously pressed keys to commands

                         ;; Static checking
                         (:name flyspell)              ;; Spell checking
                         (:name flyspell-correct)      ;; Correcting words with flyspell via custom interface
                         (:name flyspell-correct-helm) ;; Correcting words with flyspell via custom interface
                         (:name flycheck)              ;; Syntax checking

                         ;; org-mode
                         (:name org)          ;; Your life in plain text
                         (:name ob-async)     ;; Asynchronous src_block execution for org-babel
                         (:name htmlize)      ;; html org-mode reports
                         (:name org-gcal)     ;; Org sync with Google Calendar
                         (:name org-download) ;; Drag and drop images to Emacs org-mode

                         ;; (:name org-ai) ;; Minor mode for Emacs org-mode that provides access to OpenAI APIs

                         ;; Git
                         (:name magit) ;; Control Git from Emacs
 
                         ;; Tramp
                         (:name docker-tramp) ;; Tramp mode for Docker
                         (:name helm-tramp)   ;; Helm for Tramp

                         ;; Regex
                         (:name visual-regexp)          ;; A regexp/replace command for Emacs with interactive visual feedback
                         (:name visual-regexp-steroids) ;; Extends visual-regexp to support other regexp engines
                         
                         ;; Languages

                         ;; Clojure
                         (:name clojure-mode) ;; Clojure mode
                         (:name cider)        ;; REPL support
                         (:name clj-refactor) ;; Clojure refactoring functions
                         (:name cljr-helm)    ;; Helm wrapper for clj-refactor

                         ;; ;; Elm
                         (:name elm-mode)     ;; Elm mode for Emacs
                         (:name flycheck-elm) ;; Flycheck for Elm

                         ;; Markdown
                         (:name markdown-mode) ;; Markdown mode
                         (:name markdown-toc)  ;; Generate a table of contents in a markdown file

                         ;; Text
                         (:name mw-thesaurus) ;; Merriam-Webster Thesaurus

                         ;; Scala
                         (:name scala-mode) ;; Scala mode for Emacs
                         
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
