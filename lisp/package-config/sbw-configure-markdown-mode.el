(require 'use-package)

(use-package markdown-mode
  :init
  (progn)

  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-orgtbl)

    (when (eq system-type 'windows-nt)
      (setq
        markdown-command-needs-filename t
        markdown-command                "%LOCALAPPDATA%\\Pandoc\\pandoc.exe --from=markdown_github --to=html")))

  :bind
  ("C-c m p"   . markdown-preview)
  ("C-c m t 1" . markdown-insert-header-atx-1)
  ("C-c m t 2" . markdown-insert-header-atx-2)
  ("C-c m t 3" . markdown-insert-header-atx-3)
  ("C-c m t 4" . markdown-insert-header-atx-4)
  ("C-c m i"   . markdown-insert-italic)
  ("C-c m b"   . markdown-insert-bold)
  ("C-c m c"   . markdown-insert-code)
  ("C-c m l"   . markdown-insert-link)
  ("C-c m r"   . markdown-insert-reference-link-dwim))

(provide 'sbw-configure-markdown)
