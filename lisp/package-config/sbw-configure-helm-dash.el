(use-package helm-dash

  :init
  (progn
    (setq
     helm-dash-docsets-path "~/Library/Application Support/Dash/Versioned DocSets/Haskell - DHDocsetDownloader/8-4-2"
     helm-dash-common-docsets '("Haskell"))

    (    setq helm-dash-browser-func 'eww)

    )

  

  :config
  (progn)

  :bind
  ("<f2>" . helm-dash))

(provide 'sbw-configure-helm-dash)
