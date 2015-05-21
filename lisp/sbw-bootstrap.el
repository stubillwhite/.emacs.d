;; Package bootstrapping

(defmacro sbw/bootstrap--with-package-config (pkg &rest body)
  `(lexical-let* ( (name (plist-get ,pkg :name)) )
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

(provide 'sbw-bootstrap)
