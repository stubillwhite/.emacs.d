;; Add Lisp package locations
(add-to-list 'load-path "~/.emacs.d/lisp-new")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp-new/themes")

;; Bootstrap
;; ---------

;; Download everything we need
(require 'sbw-package)
(require 'sbw-package-list)

(sbw/pkg-configure-package-repositories)
(sbw/pkg-ensure-packages-are-installed sbw/pkg-all-packages)

;; Require all the core packages
(sbw/pkg-require sbw/pkg-bootstrap-packages)

;; Configure packages
(sbw/pkg-configure "~/.emacs.d/lisp-new/package-config" sbw/pkg-additional-packages)

;; Require all my packages
;(sbw/pkg-require (sbw/pkg-all-files-in-directory "FOO"))
(sbw/pkg-require (list 'sbw-cosmetics))

;; Require all my packages
;; Require tests

;; Use-package all public packages

;; Configure registers for commonly edited files
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?n '(file . "~/.emacs.d/init-new.el"))

