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
      (mapc (lambda (x) (message "Installing %s" x)(package-install x)) new-packages))))

(defconst sbw/required-packages
  '(
     ;; Standard stuff
     use-package      ;; Easy package use
     color-theme      ;; Color theme support
     undo-tree        ;; Undo tree visualisation
     auto-complete    ;; Auto-completion
     org-ac           ;; Auto-completion for org-mode
     multiple-cursors ;; Multiple cursor mode
     htmlize          ;; html org-mode reports

     ;; Lisp
     dash             ;; Modern list library
     dash-functional  ;; Further functions for dash
     f                ;; Modern file API
     s                ;; Modern string API
     
     ;; Interface
     smart-mode-line  ;; Better mode line
     helm             ;; Incremental narrowing framework
     nyan-mode        ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     
     ;; Clojure
     clojure-mode     ;; Clojure mode
     cider            ;; REPL support
     smartparens      ;; Improved paredit
     ac-cider         ;; Cider REPL autocomplete and documentation

     ;; Experimental
     ace-jump-mode    ;; Faster movement
     )
  "List of the packages required for this Emacs configuration.")

(defun sbw/install-missing-packages ()
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)
  (sbw/ensure-packages-are-installed sbw/required-packages))

(provide 'sbw-packages)
