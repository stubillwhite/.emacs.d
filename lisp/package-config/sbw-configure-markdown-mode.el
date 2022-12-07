(use-package markdown-mode
  :init
  (progn)

  :config
  (progn

    (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
    (add-hook 'markdown-mode-hook 'auto-fill-mode)

    (diminish 'org-src-mode)
    (diminish 'orgtbl-mode)
    (diminish 'eldoc-mode)
    
    (when (sbw/is-darwin?)
      (setq
       markdown-command-needs-filename nil
       markdown-command                (concat "pandoc --filter mermaid-filter --from=gfm --to=html --metadata title='markdown-preview'")
       markdown-css-paths              (list (concat user-emacs-directory "lisp/package-config/markdown-light.css"))))
    
    (when (sbw/is-windows?)
      (setq
        markdown-command-needs-filename t
        markdown-command                "%LOCALAPPDATA%\\Pandoc\\pandoc.exe --from=markdown_github --to=html"))

    (defun sbw/markdown--open-in-obsidian ()
      (interactive)
      (let* ((vault      "obsidian")
             (vault-path "/Users/white1/Dropbox/Private/obsidian")
             (file       (s-replace (concat vault-path "/") "" (buffer-file-name)))
             (url        (concat "obsidian://open?vault=" (url-hexify-string vault) "&file=" (url-hexify-string file))))
        (shell-command (concat "open '" url "'"))))

    (defun sbw/markdown--org-link-to-markdown-link ()
      (interactive)
      (let* ((regex      "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]")
             (is-org-link (looking-at regex)))
        (when is-org-link
          (let* ((url     (match-string-no-properties 1))
                 (desc    (match-string-no-properties 2))
                 (md-link (concat "[" desc "](" url ")")))
            (replace-match md-link)))))
    
    (defun sbw/markdown-reformat-org-tables ()
      (interactive)
      (print "Checking whether to reformat tables")
      (when (derived-mode-p 'markdown-mode)
        (print "Reformatting org tables")
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "-+-" nil t)
            (replace-match "-|-")))))

    (print "Adding hook")
    (add-hook 'markdown-mode-hook
              (lambda ()
                (when buffer-file-name
                  (add-hook 'before-save-hook 'sbw/markdown-reformat-org-tables)))))

  :bind
  ("M-0"       . markdown-remove-header)
  ("M-1"       . markdown-insert-header-atx-1)
  ("M-2"       . markdown-insert-header-atx-2)
  ("M-3"       . markdown-insert-header-atx-3)
  ("M-4"       . markdown-insert-header-atx-4)
  ("M-5"       . markdown-insert-header-atx-5)
  ("M-6"       . markdown-insert-header-atx-6)
  ("C-c m p"   . markdown-preview)
  ("C-c m e"   . markdown-export)
  ("C-c m h 0" . markdown-remove-header)
  ("C-c m h 1" . markdown-insert-header-atx-1)
  ("C-c m h 2" . markdown-insert-header-atx-2)
  ("C-c m h 3" . markdown-insert-header-atx-3)
  ("C-c m h 4" . markdown-insert-header-atx-4)
  ("C-c m h 5" . markdown-insert-header-atx-5)
  ("C-c m h 6" . markdown-insert-header-atx-6)
  ("C-c m i"   . markdown-insert-italic)
  ("C-c m b"   . markdown-insert-bold)
  ("C-c m c"   . markdown-insert-code)
  ("C-c m l"   . markdown-insert-link)
  ("C-c m o"   . sbw/markdown--open-in-obsidian)
  ("M-<right>" . markdown-demote)
  ("M-<left>"  . markdown-promote)
  ("C-c m r"   . markdown-insert-reference-link-dwim))

(provide 'sbw-configure-markdown-mode)

