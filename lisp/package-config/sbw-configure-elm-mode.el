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
    
    (defun sbw/elm-mode--send-current-buffer-to-repl ()
      (interactive)
      (let* ((curr (current-buffer)))
        (save-some-buffers t)
        (sbw/open-and-switch-to-window (get-buffer "*elm*"))
        (switch-to-buffer-other-window curr)
        (elm-repl-load)
        )))
  
  :bind
  (:map elm-mode-map
        ("<f5>" . sbw/elm-mode--send-current-buffer-to-repl)))

(provide 'sbw-configure-elm-mode)
