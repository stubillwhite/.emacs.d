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

    ;; Flycheck via flycheck-elm
    ;; TODO: Should be configured in flycheck configuration but that doesn't seem to be working
    (add-hook 'elm-mode-hook      'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
    
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
    (advice-add 'elm-repl-load :around #'sbw/elm-mode--ensure-vertical-split)
    (advice-add 'elm-test-project :around #'sbw/elm-mode--ensure-vertical-split)
    (advice-add 'elm-repl-push-decl :around #'sbw/elm-mode--ensure-vertical-split)

    (defun sbw/advice-save-buffers (orig-fun &rest args)
      (save-some-buffers t)
      (apply orig-fun args))
    (advice-add 'elm-mode-format-buffer :around #'sbw/advice-save-buffers)
    
    ;; Return to the original buffer when pushing
    (defun sbw/elm-mode--keep-current-buffer (orig-fun &rest args)
      (apply orig-fun args)
      (other-window -1))
    (advice-add 'elm-repl-push-decl :around #'sbw/elm-mode--keep-current-buffer)
    
    ;; Colorize elm-test output
    (defun sbw/elm-mode--colorize-elm-test-buffer ()
      (when (eq major-mode 'compilation-mode)
        (sbw/display-ansi-colors)))
    (add-hook 'compilation-filter-hook 'sbw/elm-mode--colorize-elm-test-buffer))

  :bind
  (:map elm-mode-map
        ("<f5>" . elm-repl-load)))

(provide 'sbw-configure-elm-mode)
