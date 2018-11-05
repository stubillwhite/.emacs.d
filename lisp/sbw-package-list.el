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
                         (:name diminish)            ;; Abbreviate minor mode indicators
                         (:name projectile)          ;; Project interaction library for Emacs
                         (:name powerline)           ;; Emacs version of the Vim powerline
                         (:name expand-region)       ;; Expand region by semantic units
                         (:name evil-nerd-commenter) ;; Efficient language-independent commenting
                         (:name key-chord)           ;; Map pairs of simultaneously pressed keys to commands
                         (:name hydra)               ;; Make Emacs bindings that stick around
                         (:name multiple-cursors)    ;; An experiment in adding multiple cursors to emacs
                         (:name which-key)           ;; Display available key bindings
                         (:name undo-tree)           ;; Visualise the undo tree

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
                         (:name org)      ;; Your life in plain text
                         (:name ob-async) ;; Asynchronous src_block execution for org-babel
                         (:name htmlize)  ;; html org-mode reports
                         (:name org-gcal) ;; Org sync with Google Calendar

                         ;; Git
                         (:name magit) ;; Control Git from Emacs
 
                         ;; Slack
                         (:name slack)   ;; Slack for Emacs
                         (:name emojify) ;; Emoji for Slack

                         ;; Languages

                         ;; Clojure
                         (:name clojure-mode) ;; Clojure mode
                         (:name cider)        ;; REPL support
                         (:name clj-refactor) ;; Clojure refactoring functions
                         (:name cljr-helm)    ;; Helm wrapper for clj-refactor

                         ;; ;; Elm
                         (:name elm-mode)     ;; Elm mode for emacs
                         (:name flycheck-elm) ;; Flycheck for Elm

                         ;; TODO: Implement disable
                         ;; http://haroldcarr.com/posts/2017-10-24-emacs-haskell-dev-env.html
                         ;; https://www.fosskers.ca/blog/nix-en.html
                         ;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
                         ;; Haskell
                         ;; (:name haskell-mode) ;; STACK: Haskell mode                           
                         ;; (:name hindent)      ;; STACK: 
                         ;; (:name intero)       ;; STACK: Interactive Haskell development
                         (:name dante)        ;; CABAL: Emacs mode for interactive Haskell
                         ;; TODO: Look into haskell-mode-stylish-buffer to auto-reformat

                         ;; ;; ;; JSON
                         ;; ;; json-mode ;; Major mode for editing JSON files

                         ;; Markdown
                         (:name markdown-mode) ;; Markdown mode

                         ;; YAML
                         (:name yaml-mode)         ;; Emacs major mode for editing YAML files
                         (:name flycheck-yamllint) ;; YAML checker
                         ))

(setq sbw/personal-package-list
      '(
        sbw-hash-tables
        sbw-multimethods
        sbw-value-eq
        sbw-cosmetics-code-style
        sbw-bindings
        sbw-utils
        sbw-misc
        sbw-menu
        sbw-org-config
        ))

(provide 'sbw-package-list)
