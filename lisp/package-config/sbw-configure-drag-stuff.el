(require 'use-package)

(use-package drag-stuff
  :diminish drag-stuff-mode

  :init
  (progn
    (let ( (hooks '(clojure-mode-hook
                    emacs-lisp-mode-hook
                    groovy-mode-hook
                    text-mode-hook)) )
      (dolist (hook hooks)
        (add-hook hook 'drag-stuff-mode))))

  :config
  (progn))

(provide 'sbw-configure-drag-stuff)
