;; Maximum message history, track how long initialisation takes
(setq message-log-max 16384)
(setq-default use-package-verbose t)

(defun sbw/init-message (s)
  (message (format "\n-- %s --\n" s)))

(sbw/init-message "Starting Emacs")

(defconst sbw/emacs-start-time (current-time))

(defun sbw/init-report-time-elapsed ()
  "Report time spent initialsing."
  (let ( (elapsed (float-time (time-subtract (current-time) sbw/emacs-start-time))) )
    (sbw/init-message (format "Start up completed in %.1fs" elapsed))))

(add-hook 'after-init-hook 'sbw/init-report-time-elapsed 'append)

;; Add Lisp package locations
(setq sbw/lisp-path "~/.emacs.d/lisp")
(add-to-list 'load-path sbw/lisp-path)
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

;; Bootstrap the system and install all packages
(require 'sbw-common-config)
(require 'sbw-bootstrap)
(require 'sbw-package-list)

(sbw/init-message "Initializing bootstrap")
(sbw/bootstrap-initialize)

(sbw/init-message "Installing packages")
(sbw/bootstrap-install sbw/pkg-all-packages)

(sbw/init-message "Initializing package manager")
(sbw/bootstrap-package-initialize)

(sbw/init-message "Requiring core packages")
(sbw/bootstrap-require sbw/pkg-core-packages)

(sbw/init-message "Loading package configuration")
(sbw/bootstrap-configure "~/.emacs.d/lisp/package-config" sbw/pkg-all-packages)

(sbw/init-message "Loading personal packages")
(sbw/bootstrap-require sbw/pkg-personal-packages)

(sbw/init-message "Loading tests")
(sbw/bootstrap-load-elisp-files "~/.emacs.d/lisp/test")

(sbw/init-message "Unused configurations")
(sbw/bootstrap-display-unused-configurations "~/.emacs.d/lisp/package-config" sbw/pkg-all-packages)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(custom-safe-themes
     (quote
       ("fc3930cf24b9cfbdd32d47dc552f340c20f1b9040fb2aaa39758da1719b23ac7" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))
