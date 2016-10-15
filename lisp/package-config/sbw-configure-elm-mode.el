(require 'use-package)

(use-package elm-mode
  :defer t
  :diminish elm-indent-mode

  :config
  (progn
    (add-hook 'elm-mode-hook
              (lambda ()
                (yas-minor-mode -1)))))

(provide 'sbw-configure-elm-mode)
