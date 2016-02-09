(require 'use-package)

(drag-stuff-mode 0)

(use-package drag-stuff
  :diminish drag-stuff-mode

  :init
  (progn
    ;; Enable in modes where it is useful...
    (let ( (hooks '(clojure-mode-hook
                    emacs-lisp-mode-hook
                    groovy-mode-hook
                    text-mode-hook)) )
      (dolist (hook hooks)
        (add-hook hook 'drag-stuff-mode)))

    ;; ... but ensure that org-mode is not affected
    (add-hook 'org-mode-hook (lambda () (drag-stuff-mode 0))))

  :config
  (progn))

(provide 'sbw-configure-drag-stuff)
