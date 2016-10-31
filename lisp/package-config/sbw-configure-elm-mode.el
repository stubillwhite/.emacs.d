(require 'use-package)

(use-package elm-mode
  :defer t
  :diminish elm-indent-mode

  :config
  (progn
    ;; Disable conflicting minor modes
    (add-hook 'elm-mode-hook
              (lambda ()
                (yas-minor-mode -1)))

    ;; Autocomplete via Company
    (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
    (add-to-list 'company-backends 'company-elm)

    ;; Vertical split when splitting to the REPL
    (defun sbw/elm-mode--ensure-vertical-split (orig-fun &rest args)
      (let* ( (orig-width  split-width-threshold)
              (orig-height split-height-threshold) )
        (save-some-buffers t)
        (setq split-width-threshold  0
              split-height-threshold nil)    
        (apply orig-fun args)
        (setq split-width-threshold  orig-width
              split-height-threshold orig-height)))

    (advice-add 'elm-repl-load :around #'sbw/elm-mode--ensure-vertical-split))
  
  :bind
  (:map elm-mode-map
        ("<f5>" . elm-repl-load)))

(provide 'sbw-configure-elm-mode)

