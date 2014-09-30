(require 'package)
(require 'sbw-utils)


;; This seems like a nicer approach
;; Make each setup file install if available
;  (require 'package)
;  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;                            ("marmalade" . "http://marmalade-repo.org/packages/")
;                            ("melpa" . "http://melpa.milkbox.net/packages/")))
;
;  ;; This means we prefer things from ~/.emacs.d/elpa over the standard packages.
;  (package-initialize)
;
;  ;; This bootstraps us if we don't have anything
;  (when (not package-archive-contents)
;    (package-refresh-contents))
;
;  ;; This installs elpa packages if we haven't done that yet
;  (defun maybe-install-and-require (p)
;    (when (not (package-installed-p p))
;      (package-install p))
;    (require p))
;
;  (maybe-install-and-require 'cider))


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
     multiple-cursors ;; Multiple cursor mode
     htmlize          ;; html org-mode reports

     ;; Interface
     sublimity        ;; SublimeText-style interface
     smart-mode-line  ;; Better mode line
     helm             ;; Incremental narrowing framework
     nyan-mode        ;; Nyan-nyan-nyan-nyan-nyan nyan nyan nyan
     
     ;; Clojure
     clojure-mode     ;; Clojure mode
     cider            ;; REPL support
     smartparens      ;; Improved paredit
     ac-cider         ;; Cider REPL autocomplete and documentation
     )
  "List of the packages required for this Emacs configuration.")

(defun sbw/install-missing-packages ()
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (sbw/ensure-packages-are-installed sbw/required-packages))

(provide 'sbw-packages)
