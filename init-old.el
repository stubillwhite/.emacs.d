(switch-to-buffer "*Messages*")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Install any missing pre-requisites
(require 'sbw-packages)
(sbw/install-missing-packages)
(setq-default use-package-verbose t)


;; Add custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

;; TODO This needs dash so bootstrapping is now a problem
(require 'dash)
(defun load-files-from-directory (dir)
  "Loads all the Lisp files from DIR."
  (let* ( (is-file?      (lambda (x) (not (cadr x))))
          (is-elisp?     (lambda (x) (string= (substring (car x) -3) ".el")))
          (absolute-path (lambda (x) (concat dir "/" (car x)))) )
    (-each
      (->> (directory-files-and-attributes dir nil nil nil)
        (-filter is-file?)
        (-filter is-elisp?)
        (-map absolute-path))
      (lambda (x) (load (file-name-sans-extension x))))))

(load-files-from-directory "~/.emacs.d/lisp/package-config")

;; Configure my settings
(require 'sbw-bindings)
(require 'sbw-menu)
(require 'sbw-misc)
(require 'sbw-cosmetics)
(require 'sbw-countdown)

(load-files-from-directory "~/.emacs.d/lisp/test")

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))

;; Configure registers for commonly edited files
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?n '(file . "~/.emacs.d/init-new.el"))

;; TODO Remove this
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))

