;; Turn off beginner modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Trace mode when debugging
(setq edebug-trace t)

;; Unicode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq 
  default-buffer-file-coding-system 'utf-8
  x-select-request-type             '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Backups
(setq
  temporary-file-directory "~/.emacs.d/temp"
  backup-directory-alist) '(("." . "~/.emacs.d/backups")
  auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; General settings
(setq
  inhibit-startup-message       t         ;; No splash screen
  initial-scratch-message       nil       ;; No scratch message
  visible-bell                  t         ;; No beep
  line-number-mode              t         ;; Display line position
  column-number-mode            t         ;; Display column position
  ispell-dictionary             "british" ;; British English
  shift-select-mode             nil       ;; Shift doesn't activate mark
  sentence-end-double-space     nil       ;; Sentences end in a single space
  )

;; 120 columns
(setq-default fill-column 120)

;; Shift-cursor to move between windows
(windmove-default-keybindings)

;; Remove automatic line breaking
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; Open files in running instance if possible
(server-start)

;; Backups
(defvar user-temporary-file-directory "~/.emacs-backup")
(make-directory user-temporary-file-directory t)
(setq
  backup-directory-alist            '(("." . ,(expand-file-name user-temporary-file-directory)))
  backup-by-copying                 t
  backup-directory-alist            `(("." . ,user-temporary-file-directory) (,tramp-file-name-regexp nil))
  auto-save-list-file-prefix        (concat user-temporary-file-directory ".auto-saves-")
  auto-save-file-name-transforms    `((".*" ,user-temporary-file-directory t)))

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Other file types
(add-to-list 'auto-mode-alist '("\\.db2\\'" . sql-mode))

(defun sbw/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line."
  (interactive)
  (let* ( (fill-column (point-max)) )
    (fill-paragraph nil)))

(defun sbw/unfill-region (start end)
  "Convert a multi-line region into a single line."
  (interactive "r")
  (let* ( (fill-column (point-max)) )
    (fill-region start end nil)))

(defun sbw/fix-smart-punctuation ()
  "Replace fancy punctuation characters in the current buffer with standard characters."
  (interactive)
  (let* ( (chars (sbw/ht-create
                   ?\x0091 "'"
                   ?\x0092 "'"
                   ?\x0093 "\""
                   ?\x0094 "\""
                   ?\x2014 "--"
                   ?\x2019 "'"
                   ?\x201C "\""
                   ?\x201D "\""
                   ?\x2026 "...") ))
    (save-excursion
      (-map
        (lambda (x) 
          (goto-char (point-min))
          (while (search-forward-regexp (string x) nil t)
            (replace-match (sbw/ht-get chars x) nil nil)))
        (sbw/ht-keys chars)))))

(provide 'sbw-misc)
