;; Maximum message history, track how long initialisation takes
(setq message-log-max 16384)
(setq-default use-package-verbose t)

(defun sbw/init-report-status (msg)
  (message "\n-- %s --" msg))

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

;; Download everything we need
(require 'sbw-common-config)
(require 'sbw-bootstrap)
(require 'sbw-package)
(require 'sbw-package-list)

(sbw/bootstrap-configure-repositories)
(sbw/bootstrap-install-packages sbw/pkg-all-packages)
;(sbw/pkg-configure-package-repositories)
;(sbw/pkg-ensure-packages-are-installed sbw/pkg-all-packages)

;; Require all the core packages
(sbw/pkg-require sbw/pkg-bootstrap-packages)

;; Configure packages
(sbw/pkg-configure "~/.emacs.d/lisp/package-config" sbw/pkg-all-packages)

;; Require all my packages
;; TODO Name more intuitively
(sbw/pkg-require
  (list
    'sbw-bindings
    'sbw-cosmetics
    'sbw-countdown
    'sbw-hash-tables
    'sbw-menu
    'sbw-misc
    'sbw-multimethods
    'sbw-org-review
    'sbw-time
    'sbw-utils))

;; Load all tests
(sbw/pkg-load (sbw/pkg-all-files-in-directory "~/.emacs.d/lisp/test"))
