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
     (:name cygwin-mount    :load :immediate) ;; Teach cygwin EMACS about windows styles path
   
     ;; Evil
     (:name evil            :load :immediate) ;; Welcome home
     (:name evil-leader     :load :immediate) ;; Add <leader> shortcuts to Evil, the extensible vim emulation layer
     (:name key-chord       :load :immediate) ;; Map pairs of simultaneously pressed keys to commands
     
     ;; General usability
     (:name undo-tree           :load :immediate) ;; Undo tree visualisation
     (:name flyspell            :load :immediate) ;; Spell checking
     (:name flyspell-correct    :load :immediate) ;; Correcting words with flyspell via custom interface
     (:name flycheck            :load :immediate) ;; Syntax checking
     (:name evil-nerd-commenter :load :immediate) ;; Efficient language-independent commenting
     (:name drag-stuff          :load :immediate) ;; Drag stuff around in Emacs
     (:name beacon              :load :immediate) ;; A light that follows your cursor around so you don't lose it!
     (:name keyfreq             :load :immediate) ;; Track Emacs commands frequency
     (:name macrostep           :load :immediate) ;; Interactive macro expander for Emacs
     (:name zoom-frm            :load :immediate) ;; Commands to zoom frame font size
     
     ;; org-mode
     (:name org               :load :immediate) ;; Your life in plain text
     (:name htmlize           :load :on-idle)   ;; html org-mode reports
     (:name org-gcal          :load :on-idle)   ;; Org sync with Google Calendar

     ;; Text
     (:name speed-type        :load :on-use) ;; Typing tests

     ;; Git
     (:name magit             :load :immediate) ;; Control Git from Emacs

     ;; Auto-complete
     (:name company           :load :immediate) ;; Auto-completion
     (:name yasnippet         :load :immediate) ;; Yet another snippet extension for Emacs

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
     (:name which-key         :load :immediate) ;; Display available key bindings
     (:name modalka           :load :immediate) ;; Easily introduce native modal editing of your own design
     (:name highlight-symbol  :load :immediate) ;; Quickly highlight a symbol throughout the buffer and cycle through its locations

     ;; Helm
     (:name helm              :load :immediate) ;; Incremental narrowing framework
     (:name helm-swoop        :load :immediate) ;; Efficiently skipping between matches
     (:name helm-projectile   :load :immediate) ;; Helm integration for projectile
     (:name helm-ag           :load :immediate) ;; The silver searcher with Helm interface
     (:name helm-flycheck     :load :immediate) ;; Show flycheck errors with Helm
     (:name helm-flyspell     :load :immediate) ;; Helm extension for correcting words with Flyspell
     (:name helm-dash         :load :immediate) ;; Browse Dash docsets inside Emacs
     
     ;; Elisp
     (:name json              :load :immediate) ;; JavaScript Object Notation parser / generator
     (:name async             :load :immediate) ;; Asynchronous processing in Emacs
     (:name nameless          :load :immediate) ;; Less is more. Hide package namespace in your emacs-lisp code

     ;; Languages
     
     ;; Clojure
     (:name clojure-mode      :load :immediate) ;; Clojure mode
     (:name cider             :load :immediate) ;; REPL support
     (:name smartparens       :load :immediate) ;; Improved paredit
     (:name clj-refactor      :load :immediate) ;; Clojure refactoring functions
     
     ;; Graphviz
     (:name graphviz-dot-mode :load :immediate) ;; Graphviz DOT file support and previews

     ;; Markdown
     (:name markdown-mode     :load :immediate) ;; Markdown mode

     ;; Elm
     (:name elm-mode          :load :immediate) ;; Elm mode for emacs
     
     ;; JSON
     (:name json-mode         :load :immediate) ;; Major mode for editing JSON files

     ;; Groovy
     (:name groovy-mode       :load :immediate) ;; Groovy mode

     ;; HTML
     (:name web-mode          :load :immediate) ;; Emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)
     (:name rainbow-mode      :load :immediate) ;; Colorize color names in buffers
    
     ;; Scala
     (:name scala-mode2       :load :immediate) ;; Scala mode
     
     ;; Yaml
     (:name yaml-mode         :load :immediate) ;; YAML mode

     ;; XML
     ;; (:name nxml-mode         :load :included) ;; In-built nXML mode
     )
  "List of the all packages required for this Emacs configuration.")

(defconst sbw/pkg-personal-packages
  '( (:name sbw-bindings) ;; Remove
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
     (:name sbw-org-config)
     (:name sbw-time)
     (:name sbw-utils)
     (:name sbw-value-eq) )
  "List of my packages.")

(provide 'sbw-package-list)
