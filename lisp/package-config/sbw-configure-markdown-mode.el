(require 'use-package)

(use-package markdown-mode
  :init
  (progn)

  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-orgtbl)

    (setq markdown-css-paths '("file:///Users/white1/.emacs.d/lisp/package-config/markdown.css"))
        
    (when (sbw/is-windows?)
      (setq
        markdown-command-needs-filename t
        markdown-command                "%LOCALAPPDATA%\\Pandoc\\pandoc.exe --from=markdown_github --to=html")))

  :bind
  ("C-c m p"   . markdown-preview)
  ("C-c m e"   . markdown-export)
  ("C-c m t 1" . markdown-insert-header-atx-1)
  ("C-c m t 2" . markdown-insert-header-atx-2)
  ("C-c m t 3" . markdown-insert-header-atx-3)
  ("C-c m t 4" . markdown-insert-header-atx-4)
  ("C-c m t 5" . markdown-insert-header-atx-5)
  ("C-c m t 6" . markdown-insert-header-atx-6)
  ("C-c m i"   . markdown-insert-italic)
  ("C-c m b"   . markdown-insert-bold)
  ("C-c m c"   . markdown-insert-code)
  ("C-c m h"   . markdown-insert-link)
  ("C-c m l"   . markdown-insert-list-item)
  ("C-c m r"   . markdown-insert-reference-link-dwim))

(provide 'sbw-configure-markdown)
