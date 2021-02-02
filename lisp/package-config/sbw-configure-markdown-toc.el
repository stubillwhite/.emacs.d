(use-package markdown-toc
  :init
  (progn
    (setq
     markdown-toc-header-toc-title "## Table of contents ##"))

  :config
  (progn)

  :bind
  ("C-c m t" . markdown-toc-generate-toc))

(provide 'sbw-configure-markdown-toc)
