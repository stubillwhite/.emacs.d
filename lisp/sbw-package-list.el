;; Packages required for the configuration

(defconst sbw/pkg-package-list
  '(
     ;; Core packages
     (:name use-package     :load :immediate) ;; Easy package use
     (:name dash            :load :immediate) ;; Modern list library
     (:name dash-functional :load :immediate) ;; Further functions for dash
     (:name f               :load :immediate) ;; Modern file API
     (:name s               :load :immediate) ;; Modern string API
     (:name names           :load :immediate) ;; Sane namespace handling
     (:name seq             :load :immediate) ;; Unified sequence handling

     ;; General usability
     (:name undo-tree         :load :immediate) ;; Undo tree visualisation
     (:name flyspell          :load :immediate) ;; Spell checking
     (:name flycheck          :load :immediate) ;; Syntax checking

     ;; org-mode
     (:name org               :load :immediate) ;; Your life in plain text
     (:name htmlize           :load :on-idle)   ;; html org-mode reports
     (:name org-gcal          :load :on-idle)   ;; Org sync with Google Calendar

     ;; Git
     (:name magit             :load :immediate) ;; Control Git from Emacs

     ;; Auto-complete
     (:name company           :load :immediate) ;; Auto-completion

     ;; Interface
     (:name powerline         :load :immediate) ;; Emacs version of the Vim powerline
     (:name diminish          :load :immediate) ;; Abbreviate minor mode indicators
     (:name projectile        :load :immediate) ;; Project interaction library for Emacs
     (:name avy               :load :immediate) ;; Jump to things in Emacs tree-style
     (:name ace-window        :load :immediate) ;; Quickly switch windows
     (:name golden-ratio      :load :immediate) ;; Automatic resizing of Emacs windows to the golden ratio
     (:name expand-region     :load :immediate) ;; Expand region by semantic units
     (:name hydra             :load :immediate) ;; Make Emacs bindings that stick around
     (:name multiple-cursors  :load :immediate) ;; An experiment in adding multiple cursors to emacs
     (:name key-chord         :load :immediate) ;; Map pairs of simultaneously pressed keys to commands

     ;; Helm
     (:name helm              :load :immediate) ;; Incremental narrowing framework
     (:name helm-swoop        :load :immediate) ;; Efficiently skipping between matches
     (:name helm-projectile   :load :immediate) ;; Helm integration for projectile
     
     ;; Clojure
     (:name clojure-mode      :load :immediate) ;; Clojure mode
     (:name cider             :load :immediate) ;; REPL support
     (:name smartparens       :load :immediate) ;; Improved paredit
     (:name expectations-mode :load :immediate) ;; Clojure expectations minor mode
     
     ;; Graphviz
     (:name graphviz-dot-mode :load :immediate) ;; Graphviz DOT file support and previews

     ;; Markdown
     (:name markdown-mode     :load :immediate) ;; Markdown mode

     ;; Groovy
     (:name groovy-mode       :load :immediate) ;; Groovy mode

     ;; Yaml
     (:name yaml-mode         :load :immediate) ;; YAML mode

     ;; Yasnippets
     (:name yasnippet         :load :immediate) ;; Yet another snippet extension for Emacs

     ;; Elisp
     (:name json              :load :immediate) ;; JavaScript Object Notation parser / generator
     (:name async             :load :immediate) ;; Asynchronous processing in Emacs

     ;; Vi
     (:name evil              :load :immediate) ;; Welcome home

     ;; HTML
     (:name web-mode          :load :immediate) ;; Emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)
     )
  "List of the all packages required for this Emacs configuration.")

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
