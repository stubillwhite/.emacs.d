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
      (package-refresh-contents)
      (switch-to-buffer "*Messages*")
      (mapc
        (lambda (x)
          (message "Downloading and installing package %s..." x)
          (package-install x))
        new-packages))))

(defun sbw/pkg-require (pkg-list)
  "Require the specified packages."
  (mapc
    (lambda (x)
      (message "Requiring %s" x)
      (require x))
    pkg-list))
  
(defun sbw/pkg-configure (dir pkg-list)
  "Configure the specified packages with the configuration files in DIR."
  (mapc
    (lambda (x)
      (let* ( (package (symbol-name x))
              (fnam    (concat dir "/sbw-configure-" package ".el")) )
	(message "Configuring %s" package)
        (if (f-file? fnam)
	  (load-file fnam)
	  (use-package package))))
    pkg-list))

(provide 'sbw-package)
