(use-package yasnippet
  :diminish yas-minor-mode

  :init
  (progn
    ;; (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
    (yas-global-mode)
    ;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

    (defun sbw-test-yasnippet ()
      (interactive)
      (yas-expand-snippet (yas-lookup-snippet "theatre:")))
    
    )

  :config
  (progn
    (yas-reload-all)))

(provide 'sbw-configure-yasnippet)

