;; Packages required for the configuration

(defconst sbw/pkg-repositories
  '( ("gnu"          . "http://elpa.gnu.org/packages/")
     ("org"          . "http://orgmode.org/elpa/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa"        . "http://melpa.org/packages/")
     ("marmalade"    . "http://marmalade-repo.org/packages/")
     )
  "List of cons cells of the repositories to fetch packages from.")

(defconst sbw/pkg-core-packages
  '( use-package      ;; Easy package use
     dash             ;; Modern list library
     dash-functional  ;; Further functions for dash
     f                ;; Modern file API
     s                ;; Modern string API
     names            ;; Sane namespace handling
     )
  "List of the core packages required by everything else, which have no dependencies.")

(defconst sbw/pkg-additional-packages
  '(
     ;; Standard stuff
     color-theme ;; Color theme support
     undo-tree   ;; Undo tree visualisation
     flyspell    ;; Spell checking

     ;; org-mode
     org     ;; Your life in plain text
     htmlize ;; html org-mode reports

     ;; Git
     magit ;; Control Git from Emacs

     ;; Auto-complete
     company ;; Auto-completion

     ;; Interface
     smart-mode-line ;; Better mode line
     projectile      ;; Project interaction library for Emacs
     ace-jump-mode   ;; Faster movement
     switch-window   ;; Faster switching between windows
     expand-region   ;; Expand region by semantic units
     hydra           ;; Make Emacs bindings that stick around
     worf            ;; Vi-like bindings for org-mode

     ;;nyan-mode       ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     powerline
     diminish
     
     ;; Helm
     helm            ;; Incremental narrowing framework
     helm-swoop      ;; Efficiently skipping between matches
     helm-projectile ;; Helm integration for projectile
     
     ;; Clojure
     clojure-mode      ;; Clojure mode
     cider             ;; REPL support
     smartparens       ;; Improved paredit
     ac-cider          ;; Cider REPL autocomplete and documentation
     expectations-mode ;; Clojure expectations minor mode

     ;; Graphviz
     graphviz-dot-mode ;; Graphviz DOT file support and previews

     ;; Markdown
     markdown-mode ;; Markdown mode

     ;; Groovy
     (groovy-mode . "melpa-stable") ;; Groovy mode

     ;; Yasnippets
     yasnippet ;; Yet another snippet extension for Emacs

     ;; Elisp
     json  ;; JavaScript Object Notation parser / generator
     async ;; Asynchronous processing in Emacs

     ;; XML
     ;;nxml-mode ;; Major mode for editing XML
     el-get
     )
  "List of the additional packages required for this Emacs configuration.")

(defconst sbw/pkg-all-packages
  (append sbw/pkg-core-packages sbw/pkg-additional-packages)
  "List of all the packages required for this Emacs configuration.")

(defconst sbw/pkg-personal-packages
  '( sbw-bindings
     sbw-cosmetics
     sbw-cosmetics-code-style
     sbw-countdown
     sbw-hash-tables
     sbw-menu
     sbw-misc
     sbw-multimethods
     sbw-org-review
     sbw-time
     sbw-utils )
  "List of my packages.")

(provide 'sbw-package-list)
