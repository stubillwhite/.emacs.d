;; RET should auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; General settings
(setq
  backup-directory-alist        `(("." . ,(expand-file-name "~/.emacs.d/backups")))
  color-theme-is-global         t
  diff-switches                 "-u"
  ediff-window-setup-function   'ediff-setup-windows-plain
  inhibit-startup-message       t                                                       ;; No splash screen
  initial-scratch-message       nil                                                     ;; No scratch message
  ispell-dictionary             "british"                                               ;; British English
  mouse-yank-at-point           t
  oddmuse-directory             "~/.emacs.d/oddmuse"
  save-place-file               "~/.emacs.d/places"
  sentence-end-double-space     nil
  shift-select-mode             nil
  uniquify-buffer-name-style    'forward
  visible-bell                  t                                                       ;; No beep
  whitespace-line-column        80                                                      ;; 80 columns
  whitespace-style              '(face trailing lines-tail tabs)
  )

;; Shift-cursor to move between windows
(windmove-default-keybindings)

;; Remove automatic line breaking
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; Open files in running instance if possible
(server-start)

;; Create backup files in .emacs-backup instead of everywhere
(defvar user-temporary-file-directory "~/.emacs-backup")
(make-directory user-temporary-file-directory t)
(setq
  backup-by-copying                 t
  backup-directory-alist            `(("." . ,user-temporary-file-directory) (,tramp-file-name-regexp nil))
  auto-save-list-file-prefix        (concat user-temporary-file-directory ".auto-saves-")
  auto-save-file-name-transforms    `((".*" ,user-temporary-file-directory t)))

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'sbw-misc)
