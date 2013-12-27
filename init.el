(add-to-list 'load-path "~/.emacs.d")

(require 'sbw-packages)

(sbw/pkg-install-missing-packages)

(require 'sbw-bindings)
;(require 'sbw-clojure)
(require 'sbw-misc)
(require 'sbw-cosmetics)
(require 'sbw-setup-org-mode)
(require 'sbw-setup-smex)
(require 'sbw-setup-auto-complete)
(require 'sbw-setup-flyspell)
(require 'sbw-setup-ido-ubiquitous)
