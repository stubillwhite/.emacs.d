(require 'package)
(require 'sbw-utils)

(defun sbw/packages-for-installation (pkg-list)
  "Returns a list of packages which need installation."
  (sbw/filter (lambda (x) (not (package-installed-p x))) pkg-list))

(defun sbw/ensure-packages-are-installed (pkg-list)
  "Ensure that the specified packages are installed."
  (let ((new-packages (sbw/packages-for-installation pkg-list)))
    (when new-packages
      (package-refresh-contents)
      (mapc (lambda (x) (package-install x)) new-packages))))

(defconst sbw/required-packages
  '(
     ;; Standard stuff
     use-package      ;; Easy package use
     dash             ;; Modern list library
     dash-functional  ;; Further functions for dash
     color-theme      ;; Color theme support
     undo-tree        ;; Undo tree visualisation
     ido-ubiquitous   ;; Interactive completion everywhere
     auto-complete    ;; Auto-completion
     smex             ;; M-x enhanced
     multiple-cursors ;; Multiple cursor mode
     projectile       ;; Project navigation

     ;; Clojure
     clojure-mode      ;; Clojure mode
     clojure-test-mode ;; Clojure test mode
     cider             ;; REPL support
     smartparens       ;; Improved paredit
     ac-nrepl          ;; Cider REPL autocomplete and documentation
     )
  "List of the packages required for this Emacs configuration.")

(defun sbw/install-missing-packages ()
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (sbw/ensure-packages-are-installed sbw/required-packages))

(provide 'sbw-packages)
