;; Packages required for the configuration

(defconst sbw/pkg-bootstrap-packages
  '(
     use-package      ;; Easy package use
     dash             ;; Modern list library
     dash-functional  ;; Further functions for dash
     f                ;; Modern file API
     s                ;; Modern string API
     )
  "List of the packages required to bootstrap this Emacs configuration.")

(defconst sbw/pkg-additional-packages
  '(
     ;; Standard stuff
     color-theme      ;; Color theme support
     undo-tree        ;; Undo tree visualisation
     auto-complete    ;; Auto-completion
     flyspell         ;; Spell checking

     ;; org-mode
     org              ;; Your life in plain text
     org-ac           ;; Auto-completion for org-mode
     htmlize          ;; html org-mode reports

     ;; Interface
     smart-mode-line  ;; Better mode line
     helm             ;; Incremental narrowing framework
     nyan-mode        ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     projectile       ;; Project interaction library for Emacs
     helm-projectile  ;; Helm integration for projectile
     
     ;; Clojure
     clojure-mode     ;; Clojure mode
     cider            ;; REPL support
     smartparens      ;; Improved paredit
     ac-cider         ;; Cider REPL autocomplete and documentation

     ;; Experimental
     ace-jump-mode    ;; Faster movement
     helm-swoop       ;; Efficiently skipping between matches
     )
  "List of the additional packages required for this Emacs configuration.")

(defconst sbw/pkg-all-packages
  (append sbw/pkg-bootstrap-packages sbw/pkg-additional-packages)
  "List of all the packages required for this Emacs configuration.")

(provide 'sbw-package-list)
