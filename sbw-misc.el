;; RET should auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; General settings
(setq
  inhibit-startup-message       t                   ;; No splash screen
  initial-scratch-message       nil                 ;; No scratch message
  visible-bell                  t                   ;; No beep
  color-theme-is-global         t
  sentence-end-double-space     nil
  shift-select-mode             nil
  mouse-yank-at-point           t
  uniquify-buffer-name-style    'forward
  whitespace-style              '(face trailing lines-tail tabs)
  whitespace-line-column        80
  ediff-window-setup-function   'ediff-setup-windows-plain
  oddmuse-directory             "~/.emacs.d/oddmuse"
  save-place-file               "~/.emacs.d/places"
  backup-directory-alist        `(("." . ,(expand-file-name "~/.emacs.d/backups")))
  diff-switches                 "-u")

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
