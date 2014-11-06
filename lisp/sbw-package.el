;; Package bootstrapping

(require 'package)

(defun sbw/pkg-configure-package-repositories ()
  "Configure the package to fetch packages from."
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

(defun sbw/-pkg-filter (p l)
  "Returns the items from list L for which predicate P is true."
  (delq nil
        (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/-pkg-packages-for-installation (pkg-list)
  "Returns a list of packages which need installation."
  (sbw/-pkg-filter (lambda (x) (not (package-installed-p x))) pkg-list))

(defun sbw/pkg-ensure-packages-are-installed (pkg-list)
  "Ensure that the specified packages are installed."
  (let ( (new-packages (sbw/-pkg-packages-for-installation pkg-list)) )
    (when new-packages
      (switch-to-buffer "*Messages*")
      (message "\nNew packages detected. Refreshing package list...")
      (package-refresh-contents)
      (mapc
        (lambda (x)
          (message "\nDownloading and installing package %s..." x)
          (package-install x))
        new-packages))))

(defun sbw/pkg-require (pkg-list)
  "Require the specified packages."
  (mapc
    (lambda (x)
      (message "\nRequiring %s" x)
      (require x))
    pkg-list))
  
(defun sbw/pkg-configure (dir pkg-list)
  "Configure the specified packages with the configuration files in DIR."
  (mapc
    (lambda (x)
      (let* ( (pkg  (symbol-name x))
              (fnam (concat dir "/sbw-configure-" pkg ".el")) )
	(message "\nConfiguring %s" pkg)
        (if (f-file? fnam)
	  (load-file fnam)
	  (eval `(use-package ,pkg)))))
    pkg-list))

(defun sbw/pkg-load (file-list)
  "Load the specified files"
  (-each file-list
    (lambda (x)
      (message "\nLoading %s" x)
      (load-file x))))

(defun sbw/pkg-all-files-in-directory (dir)
  "Returns a list of all the Lisp files in DIR."
  (f-files dir (lambda (x) (f-ext? x "el"))))

(provide 'sbw-package)
