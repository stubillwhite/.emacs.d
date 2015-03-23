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

(defun sbw/bootstrap--packages-for-installation (pkg-list)
  (sbw/bootstrap--filter
    (lambda (x) (not (package-installed-p x)))
    (sbw/bootstrap--package-names pkg-list)))

(defun sbw/bootstrap--pin-package (cell)
  (let* ( (pkg  (car cell))
          (repo (cdr cell)) )
    (message (format "Pinning package %s to repository %s" pkg repo))
    (setq package-pinned-packages (cons (cons pkg repo) package-pinned-packages))))

(defun sbw/bootstrap--pin-packages (pkg-list)
  (mapcar
    'sbw/bootstrap--pin-package
    (sbw/bootstrap--filter 'sbw/bootstrap--cons-cell? pkg-list)))

(defun sbw/bootstrap--new-packages? (pkg-list)
  (sbw/bootstrap--filter
    (lambda (x) (not (package-installed-p x)))
    (sbw/bootstrap--package-names pkg-list)))

(defun sbw/bootstrap--install-packages (pkg-list)
  (mapcar
    (lambda (x)
      (message "\nInstalling %s" x)
      (package-install x))
    pkg-list))

(defun sbw/bootstrap-configure-repositories ()
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)  )

(defun sbw/bootstrap-install-packages (pkg-list)
  "Install the packages from PKG-LIST if they are not already
installed. The list PKG-LIST should be a list consisting of
either package name or a cons cell of the package name and the
repository to install from for pinned packages."
  (when (sbw/bootstrap--new-packages? pkg-list)
    (switch-to-buffer "*Messages*")
    (message "\nNew packages detected. Refreshing package list...")
    (package-refresh-contents)
    (sbw/bootstrap--pin-packages pkg-list)
    (sbw/bootstrap--install-packages pkg-list)))

(provide 'sbw-bootstrap)

