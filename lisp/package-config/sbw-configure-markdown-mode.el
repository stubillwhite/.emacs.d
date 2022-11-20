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

    (defun sbw/markdown-reformat-org-tables ()
      (interactive)
      (message "Saving")
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "-|-" nil t)
          (replace-match "-|-"))))

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

(defun sbw/markdown--reload-chrome-tab ()
  (markdown-standalone)
  (shell-command "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window to reload'"))

(defun sbw/markdown--auto-reload-enable ()
  (interactive)
  (add-hook 'after-save-hook 'sbw/markdown--reload-chrome-tab))

(defun sbw/markdown--auto-reload-disable ()
  (interactive)
  (remove-hook 'after-save-hook 'sbw/markdown--reload-chrome-tab))


;; Usage Example:
;;  
;; <!-- BEGIN RECEIVE ORGTBL ${1:YOUR_TABLE_NAME} -->
;; <!-- END RECEIVE ORGTBL $1 -->
;;  
;; <!-- 
;; #+ORGTBL: SEND $1 orgtbl-to-gfm
;; | $0 | 
;; -->
(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ( (alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                org-table-last-alignment ""))
         (params2
          (list
           :splice t
           :hline (concat alignment "|")
           :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

;; (require 'org-table)
