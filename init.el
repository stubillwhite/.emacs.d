(add-to-list 'load-path "~/.emacs.d")

;; Install any missing pre-requisites
(require 'sbw-packages)
(sbw/install-missing-packages)

;; Configure my settings
(require 'sbw-bindings)
(require 'sbw-misc)
(require 'sbw-cosmetics)

;; Setup plugins
(require 'sbw-setup-ac-nrepl)
(require 'sbw-setup-auto-complete)
(require 'sbw-setup-cider)
(require 'sbw-setup-clojure-mode)
(require 'sbw-setup-flyspell)
(require 'sbw-setup-ido-ubiquitous)
(require 'sbw-setup-multiple-cursors)
(require 'sbw-setup-org-mode)
(require 'sbw-setup-projectile)
(require 'sbw-setup-smartparens)
(require 'sbw-setup-smex)
(require 'sbw-setup-undo-tree)
