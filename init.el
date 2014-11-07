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
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

;; Download everything we need
(require 'sbw-package)
(require 'sbw-package-list)

(sbw/pkg-configure-package-repositories)
(sbw/pkg-ensure-packages-are-installed sbw/pkg-all-packages)

;; Require all the core packages
(sbw/pkg-require sbw/pkg-bootstrap-packages)

;; Configure packages
(sbw/pkg-configure "~/.emacs.d/lisp/package-config" sbw/pkg-all-packages)

;; Require all my packages
(sbw/pkg-require
  (list
    'sbw-bindings
    'sbw-cosmetics
    'sbw-countdown
    'sbw-hash-tables
    'sbw-menu
    'sbw-misc
    'sbw-multimethods
    'sbw-utils))

;; Load all tests
(sbw/pkg-load (sbw/pkg-all-files-in-directory "~/.emacs.d/lisp/test"))

;; Configure registers for commonly edited files
(set-register ?e '(file . "~/.emacs.d/init.el"))

