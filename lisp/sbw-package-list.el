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
                         (:name s)               ;; Modern string API
                         (:name seq)             ;; Unified abstractions for sequences

                         ;; Completion
                         (:name helm-projectile) ;; Helm integration for projectile
                         (:name helm)            ;; Incremental narrowing framework
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
                         (:name diminish)           ;; Abbreviate minor mode indicators
                         (:name projectile)         ;; Project interaction library for Emacs
                         (:name powerline)          ;; Emacs version of the Vim powerline
                         (:name expand-region)      ;; Expand region by semantic units
                         (:name hydra)              ;; Make Emacs bindings that stick around
			 (:name which-key)          ;; Display available key bindings

                         ;; Parenthesis management
                         (:name smartparens)  ;; Improved paredit
                         
                         ;; Usability
                         (:name evil)        ;; Welcome home
                         (:name evil-leader) ;; Add <leader> shortcuts to Evil, the extensible vim emulation layer
                         (:name key-chord)   ;; Map pairs of simultaneously pressed keys to commands

                         ;; Static checking
                         (:name flyspell)              ;; Spell checking
                         (:name flyspell-correct)      ;; Correcting words with flyspell via custom interface
                         (:name flyspell-correct-helm) ;; Correcting words with flyspell via custom interface
                         (:name flycheck)              ;; Syntax checking

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
                         (:name clj-refactor) ;; Clojure refactoring functions
                         (:name cljr-helm)    ;; Helm wrapper for clj-refactor

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
        sbw-misc
        ))

(provide 'sbw-package-list)
