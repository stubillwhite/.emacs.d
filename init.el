;; Maximum message history, track how long initialisation takes
(setq message-log-max 16384)
(setq-default use-package-verbose t)

(message "-- Starting Emacs --")

(defconst sbw/emacs-start-time (current-time))

(defun sbw/init-report-time-elapsed ()
  "Report time spent initialsing."
  (let ( (elapsed (float-time (time-subtract (current-time) sbw/emacs-start-time))) )
    (message "\n-- %s --\n" (format "Start up completed in %.1fs" elapsed))))

(add-hook 'after-init-hook 'sbw/init-report-time-elapsed 'append)

;; Add Lisp package locations
(setq sbw/lisp-path "~/.emacs.d/lisp")
(add-to-list 'load-path sbw/lisp-path)
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

(setq sbw/use-new-configuration nil)

(if sbw/use-new-configuration
  (progn
    ;; Require the core scripts and bootstrap the system
    (require 'sbw-common-config)
    (require 'sbw-new-bootstrap)

    )
  (progn
    ;; Require the core scripts and bootstrap the system
    (require 'sbw-common-config)
    (require 'sbw-bootstrap)
    (require 'sbw-package-list)
    (sbw/bootstrap-configure-repositories sbw/pkg-repositories)
    (sbw/bootstrap-install-packages sbw/pkg-all-packages)

    ;; Require all the core packages
    (sbw/bootstrap-require-packages sbw/pkg-core-packages)

    ;; Configure everything now that the core is up
    (sbw/bootstrap-configure-packages "~/.emacs.d/lisp/package-config" sbw/pkg-all-packages)

    ;; Require all my packages
    (sbw/bootstrap-require-packages sbw/pkg-personal-packages)

    ;; Finally, load all tests
    (sbw/bootstrap-load-elisp-files "~/.emacs.d/lisp/test")))
