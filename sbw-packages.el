(require 'package)
(require 'sbw-utils)

(defun sbw-pkg/packages-for-installation (pkg-list)
  "Returns a list of packages which need installation."
  (sbw-utils/filter (lambda (x) (not (package-installed-p x))) pkg-list))

(defun sbw-pkg/ensure-packages-are-installed (pkg-list)
  "Ensure that the specified packages are installed."
  (let ((new-packages (sbw-pkg/packages-for-installation pkg-list)))
    (when new-packages
      (package-refresh-contents)
      (mapc (lambda (x) (package-install x)) new-packages))))

(defun sbw-pkg/install-missing-packages ()
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  (sbw-pkg/ensure-packages-are-installed
    '(
       ;; Standard stuff
       color-theme                      ;; Color theme support
       undo-tree                        ;; Undo tree visualisation
       ido-ubiquitous                   ;; Interactive completion everywhere
       auto-complete                    ;; Auto-completion
       smex                             ;; M-x enhanced
       multiple-cursors                 ;; Multiple cursor mode

       ;; Clojure
       clojure-mode                     ;; Clojure mode
       clojure-test-mode                ;; Clojure test mode
       cider                            ;; REPL support
       smartparens                      ;; Improved paredit
       ac-nrepl                         ;; Cider REPL autocomplete and documentation

       ;paredit
       ;find-file-in-project
       ;nrepl
       ;smart-tab
     )))

(provide 'sbw-packages)
