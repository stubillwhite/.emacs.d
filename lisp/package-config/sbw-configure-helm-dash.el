(use-package helm-dash

  :init
  (progn
    (setq helm-dash-browser-func 'eww))

  :config
  (progn)

  :bind
  ("<f2>" . helm-dash))

(provide 'sbw-configure-helm-dash)
