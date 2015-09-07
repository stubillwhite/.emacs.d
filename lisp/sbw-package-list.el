;; Packages required for the configuration

(defconst sbw/pkg-core-packages
  '( (:name use-package     :load :immediate) ;; Easy package use
     (:name dash            :load :immediate) ;; Modern list library
     (:name dash-functional :load :immediate) ;; Further functions for dash
     (:name f               :load :immediate) ;; Modern file API
     (:name s               :load :immediate) ;; Modern string API
     (:name names           :load :immediate) ;; Sane namespace handling
     (:name seq             :load :immediate) ;; Unified sequence handling
     )
  "List of the core packages required by everything else, which have no dependencies.")

(defconst sbw/pkg-additional-packages
  '(                                        ;; General usability
     (:name undo-tree         :load :defer) ;; Undo tree visualisation
     (:name flyspell          :load :defer) ;; Spell checking
     (:name flycheck          :load :defer) ;; Syntax checking

     ;; org-mode
     (:name org               :load :defer)     ;; Your life in plain text
     (:name htmlize           :load :on-demand) ;; html org-mode reports
     (:name org-gcal          :load :on-demand) ;; Org sync with Google Calendar

     ;; Git
     (:name magit             :load :defer) ;; Control Git from Emacs

     ;; Emacs
     (:name ert-expectations  :load :on-demand) ;; Better unit testing
     
     ;; Auto-complete
     (:name company           :load :defer) ;; Auto-completion

     ;; Interface
     (:name powerline         :load :on-demand) ;; Emacs version of the Vim powerline
     (:name diminish          :load :on-demand) ;; Abbreviate minor mode indicators
     (:name projectile        :load :immediate) ;; Project interaction library for Emacs
     (:name avy               :load :defer)     ;; Jump to things in Emacs tree-style
     (:name ace-window        :load :defer)     ;; Quickly switch windows
     (:name golden-ratio      :load :on-demand) ;; Automatic resizing of Emacs windows to the golden ratio
     (:name expand-region     :load :defer)     ;; Expand region by semantic units
     (:name hydra             :load :defer)     ;; Make Emacs bindings that stick around
     ;; (:name worf)          ;; Vi-like bindings for org-mode
     (:name multiple-cursors  :load :defer) ;; An experiment in adding multiple cursors to emacs
     (:name key-chord         :load :defer) ;; Map pairs of simultaneously pressed keys to commands

     ;; Helm
     (:name helm              :load :immediate) ;; Incremental narrowing framework
     (:name helm-swoop        :load :immediate) ;; Efficiently skipping between matches
     (:name helm-projectile   :load :immediate) ;; Helm integration for projectile
     
     ;; Clojure
     (:name clojure-mode      :load :on-demand) ;; Clojure mode
     (:name cider             :load :on-demand) ;; REPL support
     (:name smartparens       :load :on-demand) ;; Improved paredit
     (:name expectations-mode :load :on-demand) ;; Clojure expectations minor mode
     
     ;; Graphviz
     (:name graphviz-dot-mode :load :on-demand) ;; Graphviz DOT file support and previews

     ;; Markdown
     (:name markdown-mode     :load :on-demand) ;; Markdown mode

     ;; Groovy
     (:name groovy-mode       :load :on-demand) ;; Groovy mode

     ;; Yaml
     (:name yaml-mode         :load :on-demand) ;; YAML mode

     ;; Yasnippets
     (:name yasnippet         :load :defer) ;; Yet another snippet extension for Emacs

     ;; Elisp
     (:name json              :load :on-demand) ;; JavaScript Object Notation parser / generator
     (:name async             :load :on-demand) ;; Asynchronous processing in Emacs

     ;; Vi
     (:name evil              :load :defer) ;; Welcome home

     ;; XML
     ;;(:name nxml-mode         :load :on-demand) ;; Major mode for editing XML

     ;; HTML
     (:name web-mode          :load :on-demand) ;; Emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)
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
     (:name sbw-message)
     (:name sbw-org-review)
     (:name sbw-org-utils)
     (:name sbw-time)
     (:name sbw-utils)
     (:name sbw-value-eq))
  "List of my packages.")

(provide 'sbw-package-list)
