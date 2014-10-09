;; Turn off beginner modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Trace mode when debugging
(setq edebug-trace t)

;; RET should auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Unicode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-16le-dos)
(setq 
  default-buffer-file-coding-system 'utf-8
  x-select-request-type             '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; General settings
(setq
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
  whitespace-line-column        120                                                     ;; 120 columns
  whitespace-style              '(face trailing lines-tail tabs)
  sentence-end-double-space     nil                                                     ;; Sentences end in a single space
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

;; Ridiculous Windows housekeeping
;; Symantec fails to start reliably. Emacs is usually running, so if we discover that Symantec is down then smack it
;; into life. This is utterly ridiculous and should be fixed properlys somewhere else.

(defun sbw/windows-process-status (name)
  (let* ( (cmd    (concat "sc query \"" name "\""))
          (str    (shell-command-to-string cmd))
          (regex  "SERVICE_NAME: \\(.+\\)\n.*\n.*STATE\\W+: \\(\\w+\\)\\W+\\(\\w+\\)")
          (status (sbw/ht-create)) )
    (when (string-match regex str)
      (puthash :name   (match-string 1 str) status)
      (puthash :code   (match-string 2 str) status)
      (puthash :status (match-string 3 str) status)
      status)))

(defun sbw/windows-process-start (name)
  (let* ( (cmd (concat "sc start \"" name "\"")) )
    (shell-command-to-string cmd)))

(defun sbw/ensure-process-is-running (proc-name)
  (let* ( (proc-status (sbw/windows-process-status proc-name)) )
    (when (string-equal "STOPPED" (gethash :status proc-status))
      (sbw/windows-process-start proc-name)
      (message (concat "Service " proc-name " is not currently running. Starting it.")))))

(defun sbw/ensure-symantec-is-running ()
  (sbw/ensure-process-is-running "SepMasterService"))

(defun sbw/ensure-db2-is-running ()
  (sbw/ensure-process-is-running "DB2"))

(sbw/ensure-symantec-is-running)
(sbw/ensure-db2-is-running)


;; TODO - Add DB2. This is ridiculous.

(provide 'sbw-misc)
