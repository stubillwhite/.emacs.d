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
(sbw/bootstrap-install sbw/pkg-package-list)

(sbw/init-message "Initializing package manager")
(sbw/bootstrap-package-initialize)

(sbw/init-message "Loading and configuring packages")
(sbw/bootstrap-load-and-configure-packages sbw/pkg-package-list)
(sbw/bootstrap-load-deferred-packages-when-idle)

(sbw/init-message "Loading personal packages")
(sbw/bootstrap-require sbw/pkg-personal-packages)

(sbw/init-message "Loading tests")
(sbw/bootstrap-load-elisp-files "~/.emacs.d/lisp/test")

(sbw/init-message "Unused configurations")
(sbw/bootstrap-display-unused-configurations "~/.emacs.d/lisp/package-config" sbw/pkg-package-list)
