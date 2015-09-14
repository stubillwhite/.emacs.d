(require 'sbw-message)

(defvar sbw/bootstrap2--state
  (list
    :deferred-pkgs (list)
    :loader        nil
    :idle-period   5
    :idle-delay    1
    :timings       (make-hash-table :test 'equal))
  "The state of the bootstrapper.")

(defmacro sbw/bootstrap2--with-state (state &rest body)
  `(lexical-let* ( (deferred-pkgs (plist-get ,state :deferred-pkgs))
                   (loader        (plist-get ,state :loader))
                   (timings       (plist-get ,state :timings))
                   (idle-period   (plist-get ,state :idle-period))
                   (idle-delay    (plist-get ,state :idle-delay)) )
     ,@body))

(defun sbw/bootstrap2--load-package-now (pkg)
  (sbw/bootstrap2--with-package-config pkg
    (let* ( (pkg-name    (symbol-name name))
            (config-fnam (concat  "sbw-configure-" pkg-name ".el"))
            (config-dir  "~/.emacs.d/lisp/package-config") ;; TODO: REMOVE ME
            (fnam        (concat config-dir "/" config-fnam)) )
      (if (file-exists-p fnam)
        (progn
          (message (format "Loading %s" config-fnam))
          (sbw/message-enable-indent 2)
          (load-file fnam)
          (sbw/message-reset))
        (progn
          (message (format "Requiring %s" pkg-name))
          (eval `(require ',name)))))))

(defun sbw/bootstrap2--load-package-later (pkg)
  (sbw/bootstrap2--with-state sbw/bootstrap2--state
    (plist-put sbw/bootstrap2--state :deferred-pkgs (cons pkg deferred-pkgs))))

(defun sbw/bootstrap2--load-package (pkg)
  (sbw/bootstrap2--with-package-config pkg
    (cond
      ((eq load :immediate) (sbw/bootstrap2--load-package-now pkg))
      ((eq load :defer)     (sbw/bootstrap2--load-package-later pkg)))))

(defun sbw/bootstrap2-load-and-configure-packages (pkg-list)
  (mapc 'sbw/bootstrap2--load-package pkg-list))

(defun sbw/bootstrap2--schedule-loading-remaining-deferred-packages (delay)
  (sbw/bootstrap2--with-state sbw/bootstrap2--state
    (if deferred-pkgs
      (let* ( (start-time (or (current-idle-time) (seconds-to-time 0))) )
        (run-with-idle-timer
          (time-add start-time (seconds-to-time delay))
          nil
          'sbw/bootstrap2--load-deferred-package)))))

(defun sbw/bootstrap2--load-deferred-package ()
  (sbw/bootstrap2--with-state sbw/bootstrap2--state
    (sbw/bootstrap2--load-package-now (car deferred-pkgs))
    (plist-put sbw/bootstrap2--state :deferred-pkgs (cdr deferred-pkgs))
    (sbw/bootstrap2--schedule-loading-remaining-deferred-packages idle-delay)))

(defun sbw/bootstrap2-load-deferred-packages-when-idle ()
  (sbw/bootstrap2--with-state sbw/bootstrap2--state
    (sbw/bootstrap2--schedule-loading-remaining-deferred-packages idle-period)))



;;(progn
;;  (mapc 'sbw/bootstrap2--load-package sbw/pkg-all-packages)
;;  (sbw/bootstrap2-load-deferred-packages-when-idle)
;;  nil)

(defconst sbw/pkg2-package-list
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
     (:name htmlize           :load :defer)     ;; html org-mode reports
     (:name org-gcal          :load :defer)     ;; Org sync with Google Calendar

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
     (:name key-chord         :load :immediate)  ;; Map pairs of simultaneously pressed keys to commands

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

;;;;

(defmacro sbw/bootstrap2--with-package-config (pkg &rest body)
  `(lexical-let* ( (name (plist-get ,pkg :name))
                   (load (plist-get ,pkg :load)))
     ,@body))

(defun sbw/bootstrap2--filter (p l)
  (delq nil (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/bootstrap2--install-el-get ()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (message "Installing el-get")
    (with-current-buffer
      (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp))
    (el-get 'sync)))

(defun sbw/bootstrap2--configure-recipes ()
  (add-to-list 'el-get-recipe-path "~/.emacs.d/lisp/recipes"))

(defun sbw/bootstrap2-initialize ()
  "Initialise the bootstrap mechanism."
  (sbw/bootstrap2--install-el-get)
  (sbw/bootstrap2--configure-recipes))

(defun sbw/bootstrap2-install (pkg-list)
  "Install the specified packages."
  (switch-to-buffer "*Messages*")
  (mapc
    (lambda (x)
      (sbw/bootstrap2--with-package-config x
        (message (concat "Installing " (symbol-name name)))
        (eval `(el-get-bundle (symbol-name ,name)))))
    pkg-list))

(defun sbw/bootstrap2-package-initialize ()
  "Initialise the package manager."
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (package-initialize))






(defun sbw/bootstrap2-require (pkg-list)
  "Require the specified packages."
  (mapc
    (lambda (x)
      (sbw/bootstrap2--with-package-config x
        (message (concat "Requiring " (symbol-name name)))
        (eval `(require (quote ,name)))))
    pkg-list))

(defun sbw/bootstrap2-configure (dir pkg-list)
  "Configure the specified packages with the configuration files in DIR."
  (mapc
    (lambda (x)
      (sbw/bootstrap2--with-package-config x
        (let* ( (pkg  (symbol-name name))
                (fnam (concat dir "/sbw-configure-" pkg ".el")) )
          (if (f-file? fnam)
            (load-file fnam)
            (eval `(use-package ,pkg))))))
    pkg-list))

(defun sbw/bootstrap2-load-elisp-files (dir)
  "Load the Elisp files in DIR."
  (mapc
    (lambda (x) (load-file x))
    (directory-files dir :full-name ".*\.el")))

(defun sbw/bootstrap2--used-configurations (dir pkg-list)
  (mapcar
    (lambda (x) (sbw/bootstrap2--with-package-config x
             (concat "sbw-configure-" (symbol-name name) ".el")))
    pkg-list))

(defun sbw/bootstrap2--unused-configurations (dir pkg-list)
  (let* ( (used-configs (sbw/bootstrap2--used-configurations dir pkg-list))
          (is-not-used? (lambda (x) (not (member x used-configs))))
          (get-filename (lambda (x) (file-name-nondirectory x))) )
    (sbw/bootstrap2--filter
      is-not-used?
      (mapcar get-filename (directory-files dir :full-name "\.*\.el")))))

(defun sbw/bootstrap2-display-unused-configurations (dir pkg-list)
  (mapc
    (lambda (x) (message " - %s" x))
    (sbw/bootstrap2--unused-configurations dir pkg-list)))



(provide 'sbw-bootstrap2)
