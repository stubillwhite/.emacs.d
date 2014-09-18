(add-to-list 'load-path "~/.emacs.d")

;; Install any missing pre-requisites
(require 'sbw-packages)
(sbw/install-missing-packages)

;; Configure my settings
(require 'sbw-bindings)
(require 'sbw-misc)
(require 'sbw-cosmetics)

;; Configure plugins
(require 'sbw-setup-ac-nrepl)
(require 'sbw-setup-auto-complete)
(require 'sbw-setup-cider)
(require 'sbw-setup-clojure-mode)
(require 'sbw-setup-diminish)
(require 'sbw-setup-flyspell)
(require 'sbw-setup-ido-ubiquitous)
(require 'sbw-setup-multiple-cursors)
(require 'sbw-setup-org-mode)
(require 'sbw-setup-projectile)
(require 'sbw-setup-smartparens)
(require 'sbw-setup-smex)
(require 'sbw-setup-undo-tree)

;; TODO Remove this
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))
