(use-package markdown-mode
  :init
  (progn)

  :config
  (progn

    (use-package markdown-toc
      :init
      (progn
        (setq
         markdown-toc-header-toc-title "## Table of contents ##")))

    
    (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
    (add-hook 'markdown-mode-hook 'auto-fill-mode)

    (diminish 'org-src-mode)
    (diminish 'orgtbl-mode)
    (diminish 'eldoc-mode)

    (setq-default
     markdown-hide-urls        t          ;; Hide urls by default
     markdown-url-compose-char '(8230)
     )
    
    (when (sbw/is-darwin?)
      (setq
       markdown-command-needs-filename nil
       markdown-command                (concat "pandoc --filter mermaid-filter --from=gfm --to=html --metadata title='markdown-preview'")
       markdown-css-paths              (list (concat user-emacs-directory "lisp/package-config/markdown-light.css"))))
    
    (when (sbw/is-windows?)
      (setq
        markdown-command-needs-filename t
        markdown-command                "%LOCALAPPDATA%\\Pandoc\\pandoc.exe --from=markdown_github --to=html"))

    (defun sbw/markdown-preview ()
      (interactive)
      (sbw/markdown-reformat-org-tables)
      (markdown-preview))

    (defun sbw/markdown-preview-with-mermaid-diagrams ()
      (interactive)
      (lexical-let* ((input-fnam     (concat (getenv "HOME") "/trash/markdown-original.md"))
                     (md-fnam        (concat (getenv "HOME") "/trash/markdown-preview.md"))
                     (html-fnam      (concat (file-name-sans-extension md-fnam) ".html"))
                     (mermaid-fnam   (concat (if buffer-file-name (file-name-directory buffer-file-name) ".") "/mermaid-filter.err"))
                     (css-fnam       (concat user-emacs-directory "lisp/package-config/markdown-light.css"))
                     (cmd-gen-md     (concat "mmdc --outputFormat=png -i '" input-fnam "' -o '" md-fnam "'"))
                     (cmd-gen-html   (concat "pandoc --filter mermaid-filter --from=gfm --to=html --metadata title='markdown-preview' --standalone --css '" css-fnam "' -o '" html-fnam "' '" md-fnam "'"))
                     (cmd-open-html  (concat "open '" html-fnam "'")))
        (message "Formatting tables")
        (sbw/markdown-reformat-org-tables)
        (message "Saving file")
        (write-region (point-min) (point-max) input-fnam)
        (message "Generating HTML...")
        (shell-command (concat cmd-gen-md " && " cmd-gen-html " && " cmd-open-html))
        (when (and (f-exists? mermaid-fnam) (= 0 (f-size mermaid-fnam))) (f-delete mermaid-fnam))
        (message (concat "Generated " html-fnam))))
    
    (defun sbw/markdown--open-in-obsidian ()
      (interactive)
      (let* ((vault      "obsidian")
             (vault-path (file-truename "~/Library/CloudStorage/Dropbox/Private/obsidian"))
             (file       (s-replace (concat vault-path "/") "" (buffer-file-name)))
             (url        (concat "obsidian://open?vault=" (url-hexify-string vault) "&file=" (url-hexify-string file))))
        (shell-command (concat "open '" url "'"))))

    (defun sbw/markdown--to-word ()
      (interactive)
      (let* ((input-filename  buffer-file-name)
             (output-filename (concat (file-name-sans-extension input-filename) ".docx"))
             (cmd             (concat "pandoc --filter mermaid-filter --from=gfm --to=docx -o '" output-filename "' '" input-filename "'")))
        (print (concat "Creating and opening " output-filename))
        (shell-command cmd)
        (shell-command (concat "open '" output-filename "'"))))

    (defun sbw/markdown--org-link-to-markdown-link ()
      (interactive)
      (let* ((regex      "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]")
             (is-org-link (looking-at regex)))
        (when is-org-link
          (let* ((url     (match-string-no-properties 1))
                 (desc    (match-string-no-properties 2))
                 (md-link (concat "[" desc "](" url ")")))
            (replace-match md-link)))))
  
    (defun sbw/markdown--markdown-link-to-reference-link ()
      (interactive)
      (let* ((regex      "\\[\\(.+\\)\\](\\(.+\\))")
             (is-org-link (looking-at regex)))
        (when is-org-link
          (let* ((desc     (match-string-no-properties 1))
                 (url      (match-string-no-properties 2))
                 (id       (read-string "Enter reference ID: "))
                 (ref-link (s-lex-format "[${desc}][${id}]\n[${id}]: ${url}")))
            (replace-match ref-link t t)))))
    
    (defun sbw/markdown-reformat-org-tables ()
      (interactive)
      (when (derived-mode-p 'markdown-mode)
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "-+-" nil t)
            (replace-match "-|-")))))

    (defun sbw/markdown--is-dashboard? ()
      (save-excursion
        (let* ((regex     "^  - \\(dashboard\\|one-on-one\\)$"))
          (goto-char (point-min))
          (re-search-forward regex nil t))))

    (defun sbw/markdown-cleanup ()
      (interactive)
      (let* ((hidden? (outline-invisible-p (line-end-position)))) 
        (sbw/markdown-reformat-org-tables)
        (markdown-cleanup-list-numbers)
        (sbw/markdown-refresh-toc-respecting-visibility)
        (when (sbw/markdown--is-dashboard?)
          (sbw/markdown-metadata-update-date))))

    (defun sbw/markdown-refresh-toc-respecting-visibility ()
      (interactive)
      (when (markdown-toc--toc-already-present-p)
        (save-excursion
          (goto-char (markdown-toc--toc-start))
          (next-line)
          ;; Keep hidden for now
          (let* ((toc-hidden? (or 1 (outline-invisible-p (line-end-position)))))
            (markdown-toc-refresh-toc)
            (when toc-hidden?
              (next-line)
              (hide-subtree))))))
    
    (add-hook 'markdown-mode-hook
              (lambda ()
                (when buffer-file-name
                  (add-hook 'before-save-hook 'sbw/markdown-cleanup))))

    (defun sbw/markdown--insert-file-header ()
      (interactive)
      (let* ((text (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
        (markdown-insert-header 1 text)))

    (defun sbw/markdown--insert-date-header ()
      (interactive)
      (let* ((text (format-time-string "%Y-%m-%d %a")))
        (markdown-insert-header 3 text)))
    
    (defun sbw/markdown--read-tags ()
      (let ((default-directory "~/trash/list-tags")) 
        (s-split "\n" (shell-command-to-string "source bin/activate && python list-tags.py && deactivate"))))

    (defun sbw/markdown-select-tag ()
      (interactive)
      (let* ((tags (sbw/markdown--read-tags))
             (tag  (helm-comp-read "Tag:" tags)))
        tag))
    
    (defun sbw/markdown-insert-tag ()
      (interactive)
      (insert (sbw/markdown-select-tag)))

    (defun sbw/markdown-grab-text ()
      (interactive)
      (insert (shell-command-to-string "screencapture -i ~/trash/tmp.png && tesseract ~/trash/tmp.png - 2>/dev/null")))

    (defun sbw/markdown-metadata-update-date ()
      (interactive)
      (save-excursion
        (let* ((regex     "^updated: \\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\)")
               (curr-date (s-trim-right (shell-command-to-string "gdate '+%Y-%m-%d'"))))
          (goto-char (point-min))
          (when (re-search-forward regex nil t)
            (replace-match curr-date nil nil nil 1)
            (message (concat "Updated metadata date to " curr-date))))))
    )

  :bind
  ("M-0"       . markdown-remove-header)
  ("M-1"       . markdown-insert-header-atx-1)
  ("M-2"       . markdown-insert-header-atx-2)
  ("M-3"       . markdown-insert-header-atx-3)
  ("M-4"       . markdown-insert-header-atx-4)
  ("M-5"       . markdown-insert-header-atx-5)
  ("M-6"       . markdown-insert-header-atx-6)
  ("C-c m b"   . markdown-insert-bold)
  ("C-c m c"   . markdown-insert-code)
  ("C-c m e"   . markdown-export)
  ("C-c m h 0" . markdown-remove-header)
  ("C-c m h 1" . markdown-insert-header-atx-1)
  ("C-c m h 2" . markdown-insert-header-atx-2)
  ("C-c m h 3" . markdown-insert-header-atx-3)
  ("C-c m h 4" . markdown-insert-header-atx-4)
  ("C-c m h 5" . markdown-insert-header-atx-5)
  ("C-c m h 6" . markdown-insert-header-atx-6)
  ("C-c m h d" . sbw/markdown--insert-date-header)
  ("C-c m h f" . sbw/markdown--insert-file-header)
  ("C-c m i"   . markdown-insert-italic)
  ("C-c m l"   . markdown-insert-link)
  ("C-c m m d" . sbw/markdown-metadata-update-date)
  ("C-c m o"   . sbw/markdown--open-in-obsidian)
  ("C-c m p"   . sbw/markdown-preview-with-mermaid-diagrams)
  ("C-c m r"   . markdown-insert-reference-link-dwim)
  ("C-c m s"   . markdown-insert-strike-through)
  ("C-c m t"   . markdown-toc-generate-toc)  
  ("C-c m w"   . sbw/markdown--to-word)
  ("M-<left>"  . markdown-promote)
  ("M-<right>" . markdown-demote)
  )

(provide 'sbw-configure-markdown-mode)


