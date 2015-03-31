;; Package bootstrapping

(require 'package)

;; Look into el-get-sources

(defconst sbw/new-pkg-base-packages
  '( (:name el-get          :type :package) ;; Manage external elisp dependencies
     (:name use-package     :type :package) ;; Easy package use
     (:name dash            :type :package) ;; Modern list library
     (:name dash-functional :type :package) ;; Further functions for dash
     (:name f               :type :package) ;; Modern file API
     (:name s               :type :package) ;; Modern string API
     (:name names           :type :package) ;; Sane namespace handling
     )
  "The base packages upon which the other installation scripts depend.")

(defconst sbw/new-pkg-additional-packages
  '(
     ;; Interface
     (:name color-theme     :type :package) ;; Color theme support
     (:name undo-tree       :type :package) ;; Undo tree visualisation
     (:name flyspell        :type :package) ;; Spell checking
     (:name smart-mode-line :type :package) ;; Better mode line
     (:name nyan-mode       :type :package) ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     (:name projectile      :type :package) ;; Project interaction library for Emacs
     (:name ace-jump-mode   :type :package) ;; Faster movement
     (:name switch-window   :type :package) ;; Faster switching between windows
     (:name expand-region   :type :package) ;; Expand region by semantic units

     ;; org-mode
     (:name org     :type :package) ;; Your life in plain text
     (:name htmlize :type :package) ;; html org-mode reports

     ;; Git
     (:name magit :type :package) ;; Control Git from Emacs

     ;; Auto-complete
     (:name company :type :package) ;; Auto-completion

     ;; Helm
     (:name helm            :type :package) ;; Incremental narrowing framework
     (:name helm-swoop      :type :package) ;; Efficiently skipping between matches
     (:name helm-projectile :type :package) ;; Helm integration for projectile
     
     ;; Clojure
     (:name clojure-mode      :type :package) ;; Clojure mode
     (:name cider             :type :package) ;; REPL support
     (:name smartparens       :type :package) ;; Improved paredit
     (:name ac-cider          :type :package) ;; Cider REPL autocomplete and documentation
     (:name marmalade         :type :package) ;; Clojure expectations minor mode
     (:name expectations-mode :type :git :url "https://github.com/gar3thjon3s/expectations-mode.git") ;; Minor mode for expectations

     ;; Graphviz
     (:name graphviz-dot-mode :type :package) ;; Graphviz DOT file support and previews

     ;; Markdown
     (:name markdown-mode :type :package) ;; Markdown mode

     ;; Groovy
     (:name groovy-mode :type :package) ;; Groovy mode

     ;; Yasnippets
     (:name yasnippet :type :package) ;; Yet another snippet extension for Emacs

     ;; Elisp
     (:name json  :type :package) ;; JavaScript Object Notation parser / generator
     (:name async :type :package) ;; Asynchronous processing in Emacs

     ;; XML
     ;;nxml-mode ;; Major mode for editing XML
     )
  "The additional packages required.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sbw/new-bootstrap--with-package-config (pkg &rest body)
  `(let* ( (name (plist-get ,pkg :name))
           (type (plist-get ,pkg :type))
           (repo (plist-get ,pkg :repo)) )
     ,@body))

(defun sbw/new-bootstrap--filter (p l)
  (delq nil (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/new-bootstrap--pin-package (pkg)
  (sbw/new-bootstrap--with-package-config pkg
    (when (and (eq type :package) (not (eq repo nil)))
      (message "Pinning package '%s' to repository '%s'" name repo)
      (setq package-pinned-packages (cons (cons name repo) package-pinned-packages)))))

(defun sbw/new-bootstrap--install-using-package (pkg)
  (sbw/new-bootstrap--with-package-config pkg
    (when (and (eq type :package) (not (eq repo nil)))
      (message "Pinning package '%s' to repository '%s'" name repo)
      (setq package-pinned-packages (cons (cons name repo) package-pinned-packages)))
    (message "Installing '%s' using package" name)
    (package-install name)))

(defun sbw/new-bootstrap--install-using-el-get (pkg)
  (sbw/new-bootstrap--with-package-config pkg
    (message "Installing '%s' using el-get" name)
    (el-get-bundle name :type type)))

(defun sbw/new-bootstrap--install-package (pkg)
  (sbw/new-bootstrap--with-package-config pkg
    (cond
      ((eq type :package) (sbw/new-bootstrap--install-using-package pkg))
      ((eq type :git)     (sbw/new-bootstrap--install-using-el-get pkg))
      (:else              (error "Unknown package type '%s'" type)))))

(defun sbw/new-bootstrap--should-install? (pkg)
  (sbw/new-bootstrap--with-package-config pkg
    (not (package-installed-p name))))

(defun sbw/new-bootstrap-install (pkg-list)
  (mapc
    'sbw/new-bootstrap--install-package
    (sbw/new-bootstrap--filter 'sbw/new-bootstrap--should-install? pkg-list)))

(sbw/new-bootstrap-install sbw/new-pkg-base-packages)
(sbw/new-bootstrap-install sbw/new-pkg-additional-packages)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'el-get-recipe-path "~/.emacs.d/lisp/recipes")
(el-get 'sync)

(el-get-bundle marmalade)
