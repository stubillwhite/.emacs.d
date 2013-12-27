

;; RET should auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; General settings
(setq inhibit-startup-message t)                 ;; No splash screen
(setq initial-scratch-message nil)               ;; No scratch message

;; Remove automatic line breaking
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(require 'undo-tree)
(global-undo-tree-mode t)                        ;; Use undo-tree

(server-start)                                   ;; Open files in running instance if possible

;; Create backup files in .emacs-backup instead of everywhere
(defvar user-temporary-file-directory "~/.emacs-backup")
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist 
      `(("." . ,user-temporary-file-directory) (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t)))


(setq 
	  visible-bell                 t
      inhibit-startup-message      t
      color-theme-is-global        t
      sentence-end-double-space    nil
      shift-select-mode            nil
      mouse-yank-at-point          t
      uniquify-buffer-name-style   'forward
      whitespace-style             '(face trailing lines-tail tabs)
      whitespace-line-column       80
      ediff-window-setup-function  'ediff-setup-windows-plain
      oddmuse-directory            "~/.emacs.d/oddmuse"
      save-place-file              "~/.emacs.d/places"
      backup-directory-alist       `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches                "-u")

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous t)
(setq
      ido-enable-prefix                      nil
      ido-enable-flex-matching               t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer                  'always
      ido-use-filename-at-point              'guess
      ido-use-virtual-buffers                t
      ido-handle-duplicate-virtual-buffers   2
      ido-max-prospects                      10)

(provide 'sbw-misc)
