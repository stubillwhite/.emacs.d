;; Package bootstrapping

(defconst sbw/new-bootstrap-core-packages
  '( (:name use-package)     ;; Easy package use
     (:name dash)            ;; Modern list library
     (:name dash-functional) ;; Further functions for dash
     (:name f)               ;; Modern file API
     (:name s)               ;; Modern string API
     (:name names)           ;; Sane namespace handling
     (:name seq)             ;; Unified sequence handling
     )
  "List of the core packages required by everything else, which have no dependencies.")

(defconst sbw/new-bootstrap-additional-packages
  '( (:name color-theme) ;; Color theme support
     (:name undo-tree)   ;; Undo tree visualisation
     (:name flyspell)    ;; Spell checking

     ;; org-mode
     (:name org)     ;; Your life in plain text
     (:name htmlize) ;; html org-mode reports

     ;; Git
     (:name magit) ;; Control Git from Emacs

     ;; Auto-complete
     (:name company) ;; Auto-completion

     ;; Interface
     ;;smart-mode-line ;; Better mode line
     ;;smart-mode-line-powerline-theme ;; Powerline theme for smart-mode-line
     (:name projectile)      ;; Project interaction library for Emacs
     (:name ace-jump-mode)   ;; Faster movement
     (:name switch-window)   ;; Faster switching between windows
     (:name expand-region)   ;; Expand region by semantic units
     (:name hydra)           ;; Make Emacs bindings that stick around
     ;;(:name worf)            ;; Vi-like bindings for org-mode

     ;;nyan-mode       ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     (:name powerline)
     (:name diminish)
     
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

     ;; XML
     ;;nxml-mode ;; Major mode for editing XML
     )
  "List of the additional packages required for this Emacs configuration.")

(defconst sbw/new-bootstrap-all-packages
  (append sbw/new-bootstrap-core-packages sbw/new-bootstrap-additional-packages)
  "List of all the packages required for this Emacs configuration.")

(defconst sbw/new-bootstrap-personal-packages
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
     (:name sbw-utils) )
  "List of my packages.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sbw/new-bootstrap--with-package-config (pkg &rest body)
  `(lexical-let* ( (name (plist-get ,pkg :name)) )
     ,@body))

(defun sbw/new-bootstrap--filter (p l)
  (delq nil (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/new-bootstrap--install-el-get ()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (message "Installing el-get")
    (with-current-buffer
      (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp))
    (el-get 'sync)))

(defun sbw/new-bootstrap--configure-recipes ()
  (add-to-list 'el-get-recipe-path "~/.emacs.d/lisp/recipes"))

(defun sbw/new-bootstrap-initialize ()
  "Initialise the bootstrap mechanism."
  (sbw/new-bootstrap--install-el-get)
  (sbw/new-bootstrap--configure-recipes))

(defun sbw/new-bootstrap-install (pkg-list)
  "Install the specified packages."
  (switch-to-buffer "*Messages*")
  (mapc
    (lambda (x)
      (sbw/new-bootstrap--with-package-config x
        (message (concat "Installing " (symbol-name name)))
        (eval `(el-get-bundle (symbol-name ,name)))))
    pkg-list))

(defun sbw/new-bootstrap-require (pkg-list)
  "Require the specified packages."
  (mapc
    (lambda (x)
      (sbw/new-bootstrap--with-package-config x
        (message (concat "Requiring " (symbol-name name)))
        (eval `(require (quote ,name)))
        ))
    pkg-list))

(defun sbw/new-bootstrap-configure (dir pkg-list)
  "Configure the specified packages with the configuration files in DIR."
  (mapc
    (lambda (x)
      (sbw/new-bootstrap--with-package-config x
        (let* ( (pkg  (symbol-name name))
                (fnam (concat dir "/sbw-configure-" pkg ".el")) )
          (if (f-file? fnam)
            (load-file fnam)
            (eval `(use-package ,pkg))))))
    pkg-list))

(defun sbw/new-bootstrap-load-elisp-files (dir)
  "Load the Elisp files in DIR."
  (mapc
    (lambda (x) (load-file x))
    (directory-files dir :full-name ".*\.el")))


(provide 'new-bootstrap)
