;; Packages required for the configuration

(defconst sbw/pkg-core-packages
  '( (:name use-package)     ;; Easy package use
     (:name dash)            ;; Modern list library
     (:name dash-functional) ;; Further functions for dash
     (:name f)               ;; Modern file API
     (:name s)               ;; Modern string API
     (:name names)           ;; Sane namespace handling
     (:name seq)             ;; Unified sequence handling
     )
  "List of the core packages required by everything else, which have no dependencies.")

(defconst sbw/pkg-additional-packages
  '( ;; General usability
     (:name color-theme) ;; Color theme support
     (:name undo-tree)   ;; Undo tree visualisation
     (:name flyspell)    ;; Spell checking
     (:name flycheck)    ;; Syntax checking

     ;; org-mode
     (:name org)      ;; Your life in plain text
     (:name htmlize)  ;; html org-mode reports
     (:name org-gcal) ;; Org sync with Google Calendar

     ;; Git
     (:name magit) ;; Control Git from Emacs

     ;; Auto-complete
     (:name company) ;; Auto-completion

     ;; Interface
     (:name powerline)     ;; Emacs version of the Vim powerline
     (:name diminish)      ;; Abbreviate minor mode indicators
     (:name projectile)    ;; Project interaction library for Emacs
     (:name ace-jump-mode) ;; Faster movement
     ;;(:name ace-window)    ;; Quickly switch windows
     (:name golden-ratio)  ;; Automatic resizing of Emacs windows to the golden ratio
     (:name switch-window) ;; Faster switching between windows
     (:name expand-region) ;; Expand region by semantic units
     (:name hydra)         ;; Make Emacs bindings that stick around
     ;; (:name worf)          ;; Vi-like bindings for org-mode

     ;; Helm
     (:name helm)            ;; Incremental narrowing framework
     (:name helm-swoop)      ;; Efficiently skipping between matches
     (:name helm-projectile) ;; Helm integration for projectile
     
     ;; Clojure
     (:name clojure-mode)      ;; Clojure mode
     (:name cider)             ;; REPL support
     (:name smartparens)       ;; Improved paredit
     (:name ac-cider)          ;; Cider REPL autocomplete and documentation
     (:name expectations-mode) ;; Clojure expectations minor mode

     ;; Graphviz
     (:name graphviz-dot-mode) ;; Graphviz DOT file support and previews

     ;; Markdown
     (:name markdown-mode) ;; Markdown mode

     ;; Groovy
     (:name groovy-mode) ;; Groovy mode

     ;; Yasnippets
     (:name yasnippet) ;; Yet another snippet extension for Emacs

     ;; Elisp
     (:name json)  ;; JavaScript Object Notation parser / generator
     (:name async) ;; Asynchronous processing in Emacs

     ;; Vi
     (:name evil) ;; Welcome home

     ;; XML
     ;;(:name nxml-mode) ;; Major mode for editing XML

     ;; HTML
     (:name web-mode) ;; Emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)
     )
  "List of the additional packages required for this Emacs configuration.")

(defconst sbw/pkg-all-packages
  (append sbw/pkg-core-packages sbw/pkg-additional-packages)
  "List of all the packages required for this Emacs configuration.")

(defconst sbw/pkg-personal-packages
  '( (:name sbw-bindings)
     (:name sbw-cosmetics)
     (:name sbw-cosmetics-code-style)
     (:name sbw-countdown)
     (:name sbw-hash-tables)
     (:name sbw-menu)
     (:name sbw-misc)
     (:name sbw-multimethods)
     (:name sbw-org-review)
     (:name sbw-time)
     (:name sbw-utils)
     (:name sbw-value-eq))
  "List of my packages.")

(provide 'sbw-package-list)
