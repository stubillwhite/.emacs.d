(use-package evil-smartparens
  :diminish evil-smartparens
  
  :init
  (progn
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

  :config
  (progn))

(provide 'sbw-configure-evil-smartparens)


