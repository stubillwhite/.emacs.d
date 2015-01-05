;; Packages required for the configuration

;; TODO Put all these in
;(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
;                           ("gnu" . "http://elpa.gnu.org/packages/")
;                           ("melpa" . "http://melpa.org/packages/")
;                           ("melpa-stable" . "http://stable.melpa.org/packages/")
;                           ("marmalade" . "http://marmalade-repo.org/packages/")
;                           ))

(defconst sbw/pkg-bootstrap-packages
  '(
     use-package      ;; Easy package use
     dash             ;; Modern list library
     dash-functional  ;; Further functions for dash
     f                ;; Modern file API
     s                ;; Modern string API
     names            ;; Sane namespace handling
     )
  "List of the packages required to bootstrap this Emacs configuration.")

(defconst sbw/pkg-additional-packages
  '(
     ;; Standard stuff
     color-theme            ;; Color theme support
     undo-tree              ;; Undo tree visualisation
     flyspell               ;; Spell checking

     ;; org-mode
     org                    ;; Your life in plain text
     htmlize                ;; html org-mode reports

     ;; Auto-complete
     company                ;; Auto-completion
     
     ;; Interface
     smart-mode-line        ;; Better mode line
     nyan-mode              ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     projectile             ;; Project interaction library for Emacs
     ace-jump-mode          ;; Faster movement
     switch-window          ;; Faster switching between windows

     ;; Helm
     helm                   ;; Incremental narrowing framework
     helm-swoop             ;; Efficiently skipping between matches
     helm-projectile        ;; Helm integration for projectile
     
     ;; Clojure
     clojure-mode           ;; Clojure mode
     cider                  ;; REPL support
     smartparens            ;; Improved paredit
     ac-cider               ;; Cider REPL autocomplete and documentation

     ;; Graphviz
     graphviz-dot-mode      ;; Graphviz DOT file support and previews

     ;; Groovy
;;     (groovy-mode . "melpa") ;; Groovy mode
     groovy-mode            ;; Groovy mode
     
     ;; Experimental
     )
  "List of the additional packages required for this Emacs configuration.")

(defconst sbw/pkg-all-packages
  (append sbw/pkg-bootstrap-packages sbw/pkg-additional-packages)
  "List of all the packages required for this Emacs configuration.")

(setq package-pinned-packages  '((groovy-mode . "melpa")))

(provide 'sbw-package-list)
