;; Package bootstrapping

(require 'package)

(defun sbw/bootstrap--filter (p l)
  (delq nil (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/bootstrap--cons-cell? (x)
  (and (listp x) (not (listp (cdr x)))))

(defun sbw/bootstrap--package-names (pkg-list)
  (mapcar
    (lambda (x) (if (sbw/bootstrap--cons-cell? x) (car x) x))
    pkg-list))

(defun sbw/bootstrap--pin-package (cell)
  (let* ( (pkg  (car cell))
          (repo (cdr cell)) )
    (message (format "Pinning package %s to repository %s" pkg repo))
    (setq package-pinned-packages (cons (cons pkg repo) package-pinned-packages))))

(defun sbw/bootstrap--pin-packages (pkg-list)
  (mapcar
    'sbw/bootstrap--pin-package
    (sbw/bootstrap--filter 'sbw/bootstrap--cons-cell? pkg-list)))

(defun sbw/bootstrap--packages-to-install (pkg-list)
  (sbw/bootstrap--filter
    (lambda (x) (not (package-installed-p x)))
    (sbw/bootstrap--package-names pkg-list)))

(defun sbw/bootstrap--install-packages (pkg-list)
  (mapcar
    (lambda (x)
      (message "\nInstalling %s" x)
      (package-install x))
    pkg-list))

(defun sbw/bootstrap-configure-repositories (repos)
  (mapcar
    (lambda (x) (add-to-list 'package-archives x t))
    repos)
  (package-initialize))

(defun sbw/bootstrap-install-packages (pkg-list)
  "Install the packages from PKG-LIST if they are not already
installed. The list PKG-LIST should be a list consisting of
either package name or a cons cell of the package name and the
repository to install from for pinned packages."
  (let* ( (pkgs-to-install (sbw/bootstrap--packages-to-install pkg-list)) )
    (when pkgs-to-install
      (switch-to-buffer "*Messages*")
      (message "\nNew packages detected. Refreshing package list...")
      (package-refresh-contents)
      (sbw/bootstrap--pin-packages pkg-list)
      (sbw/bootstrap--install-packages pkgs-to-install))))

(defun sbw/bootstrap-require-packages (pkg-list)
  "Require the specified packages."
  (mapc
    (lambda (x) (require x))
    (sbw/bootstrap--package-names pkg-list)))
  
(defun sbw/bootstrap-configure-packages (dir pkg-list)
  "Configure the specified packages with the configuration files in DIR."
  (mapc
    (lambda (x)
      (let* ( (pkg  (symbol-name x))
              (fnam (concat dir "/sbw-configure-" pkg ".el")) )
        (if (f-file? fnam)
          (load-file fnam)
          (eval `(use-package ,pkg)))))
    (sbw/bootstrap--package-names pkg-list)))

(defun sbw/bootstrap-load-elisp-files (dir)
  "Load the Elisp files in DIR."
  (mapc
    (lambda (x) (load-file x))
    (directory-files dir :full-name ".*\.el")))

(provide 'sbw-bootstrap)

