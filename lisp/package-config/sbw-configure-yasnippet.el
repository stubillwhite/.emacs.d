(use-package yasnippet
  :diminish yas-minor-mode

  :init
  (progn
    ;; (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
    (yas-global-mode)
    (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))

  :config
  (progn
    (yas-reload-all)))

(provide 'sbw-configure-yasnippet)

