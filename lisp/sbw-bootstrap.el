(require 'sbw-message)

;; Prevent the built-in package manager from customising my files
(setq custom-file "~/.emacs.d/emacs-customisations.el")
(load custom-file)

(defvar sbw/bootstrap--state
  (list
    :on-idle-pkgs (list)
    :loader       nil
    :idle-period  5
    :idle-delay   1
    :timings      (make-hash-table :test 'equal))
  "The state of the bootstrapper.")

(defmacro sbw/bootstrap--with-state (state &rest body)
  `(lexical-let* ( (on-idle-pkgs (plist-get ,state :on-idle-pkgs))
                   (loader       (plist-get ,state :loader))
                   (timings      (plist-get ,state :timings))
                   (idle-period  (plist-get ,state :idle-period))
                   (idle-delay   (plist-get ,state :idle-delay)) )
     ,@body))

(defun sbw/bootstrap--load-package-now (pkg)
  (sbw/bootstrap--with-package-config pkg
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

(defun sbw/bootstrap--load-package-later (pkg)
  (sbw/bootstrap--with-state sbw/bootstrap--state
    (plist-put sbw/bootstrap--state :on-idle-pkgs (cons pkg on-idle-pkgs))))

(defun sbw/bootstrap--load-package (pkg)
  (sbw/bootstrap--with-package-config pkg
    (cond
      ((eq load :immediate) (sbw/bootstrap--load-package-now pkg))
      ((eq load :on-idle)   (sbw/bootstrap--load-package-later pkg)))))

(defun sbw/bootstrap-load-and-configure-packages (pkg-list)
  (mapc 'sbw/bootstrap--load-package pkg-list))

(defun sbw/bootstrap--schedule-loading-remaining-on-idle-packages (delay)
  (sbw/bootstrap--with-state sbw/bootstrap--state
    (when on-idle-pkgs
      (let* ( (start-time (or (current-idle-time) (seconds-to-time 0))) )
        (run-with-idle-timer
          (time-add start-time (seconds-to-time delay))
          nil
          'sbw/bootstrap--load-on-idle-package)))))

(defun sbw/bootstrap--load-on-idle-package ()
  (sbw/bootstrap--with-state sbw/bootstrap--state
    (when on-idle-pkgs
      (sbw/bootstrap--load-package-now (car on-idle-pkgs))
      (plist-put sbw/bootstrap--state :on-idle-pkgs (cdr on-idle-pkgs))
      (sbw/bootstrap--schedule-loading-remaining-on-idle-packages idle-delay))))

(defun sbw/bootstrap-load-on-idle-packages-when-idle ()
  (sbw/bootstrap--with-state sbw/bootstrap--state
    (sbw/bootstrap--schedule-loading-remaining-on-idle-packages idle-period)))

(defun sbw/bootstrap-load-on-idle-packages-now ()
  (interactive)
  (plist-put sbw/bootstrap--state :idle-period 0)
  (plist-put sbw/bootstrap--state :idle-delay  0)
  (sbw/bootstrap-load-on-idle-packages-when-idle))

(defmacro sbw/bootstrap--with-package-config (pkg &rest body)
  `(lexical-let* ( (name (plist-get ,pkg :name))
                   (load (plist-get ,pkg :load)) )
     ,@body))

(defun sbw/bootstrap--filter (p l)
  (delq nil (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/bootstrap--install-el-get ()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (message "Installing el-get")
    (with-current-buffer
      (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp))
    (el-get 'sync)))

(defun sbw/bootstrap--configure-recipes ()
  (add-to-list 'el-get-recipe-path "~/.emacs.d/lisp/recipes"))

(defun sbw/bootstrap-initialize ()
  "Initialise the bootstrap mechanism."
  (sbw/bootstrap--install-el-get)
  (sbw/bootstrap--configure-recipes))

(defun sbw/bootstrap-install (pkg-list)
  "Install the specified packages."
  (switch-to-buffer "*Messages*")
  (mapc
    (lambda (x)
      (sbw/bootstrap--with-package-config x
        (message (concat "Installing " (symbol-name name)))
        (eval `(el-get-bundle (symbol-name ,name)))))
    pkg-list))

(defun sbw/bootstrap-package-initialize ()
  "Initialise the package manager."
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (package-initialize))

(defun sbw/bootstrap-require (pkg-list)
  "Require the specified packages."
  (mapc
    (lambda (x)
      (sbw/bootstrap--with-package-config x
        (message (concat "Requiring " (symbol-name name)))
        (eval `(require (quote ,name)))))
    pkg-list))

(defun sbw/bootstrap-configure (dir pkg-list)
  "Configure the specified packages with the configuration files in DIR."
  (mapc
    (lambda (x)
      (sbw/bootstrap--with-package-config x
        (let* ( (pkg  (symbol-name name))
                (fnam (concat dir "/sbw-configure-" pkg ".el")) )
          (if (f-file? fnam)
            (load-file fnam)
            (eval `(use-package ,pkg))))))
    pkg-list))

(defun sbw/bootstrap-load-elisp-files (dir)
  "Load the Elisp files in DIR."
  (mapc
    (lambda (x) (load-file x))
    (directory-files dir :full-name ".*\.el")))

(defun sbw/bootstrap--used-configurations (dir pkg-list)
  (mapcar
    (lambda (x) (sbw/bootstrap--with-package-config x
             (concat "sbw-configure-" (symbol-name name) ".el")))
    pkg-list))

(defun sbw/bootstrap--unused-configurations (dir pkg-list)
  (let* ( (used-configs (sbw/bootstrap--used-configurations dir pkg-list))
          (is-not-used? (lambda (x) (not (member x used-configs))))
          (get-filename (lambda (x) (file-name-nondirectory x))) )
    (sbw/bootstrap--filter
      is-not-used?
      (mapcar get-filename (directory-files dir :full-name "\.*\.el")))))

(defun sbw/bootstrap-display-unused-configurations (dir pkg-list)
  (mapc
    (lambda (x) (message " - %s" x))
    (sbw/bootstrap--unused-configurations dir pkg-list)))

(provide 'sbw-bootstrap)
