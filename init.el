;; Hack to override local loading

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Normal configuration

;; Configure paths
(setq sbw/lisp-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path sbw/lisp-path)
(add-to-list 'load-path (concat sbw/lisp-path "/test"))
(add-to-list 'custom-theme-load-path (concat sbw/lisp-path "/themes"))

(print sbw/lisp-path)

;; Bootstrap packges
(require 'sbw-common-config)
(require 'sbw-bootstrap)
(require 'sbw-package-list)
(require 'sbw-cosmetics)

(sbw/bootstrap-init)
(sbw/bootstrap-packages sbw/package-list)

(sbw/bootstrap-require sbw/personal-package-list)
