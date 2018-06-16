(require 'cl)

(setq sbw/bootstrap--buffer-name "*Bootstrap*")

;; Prevent the built-in package manager from customising my files
(setq custom-file "~/.emacs.d/emacs-customisations.el")
(load custom-file)

(defun sbw/bootstrap-load-secrets ()
  "Load encrypted secrets."
  (interactive)
  (require 'secrets "secrets.el.gpg"))

(defun sbw/bootstrap--install-straight-if-required ()
  (let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)))

(defun sbw/bootstrap--display-blank-status-buffer ()
  (get-buffer-create sbw/bootstrap--buffer-name)
  (switch-to-buffer sbw/bootstrap--buffer-name)
  (with-current-buffer sbw/bootstrap--buffer-name
    (read-only-mode -1)
    (erase-buffer))
  (set-buffer sbw/bootstrap--buffer-name))

(defun sbw/bootstrap--display-package-statuses (pkg pkgs statuses)
  (sbw/bootstrap--display-blank-status-buffer)
  (insert "Bootstrapping packages\n----------------------\n\n")
  (loop for k in pkgs
        do (with-current-buffer sbw/bootstrap--buffer-name
             (let* ((name (symbol-name (sbw/bootstrap--pkg-name k))))
               (insert (format "%s %s\n" (gethash name statuses) name)))))
  (goto-line (+ 4 (cl-position pkg pkgs)))
  (redisplay t))

(defun sbw/bootstrap--update-package-status (pkg pkgs new-status statuses)
  (puthash (symbol-name (sbw/bootstrap--pkg-name pkg)) new-status statuses)
  (sbw/bootstrap--display-package-statuses pkg pkgs statuses))

(defun sbw/bootstrap-init ()
  "Configure bootstrap."
  (get-buffer-create sbw/bootstrap--buffer-name)
  (sbw/bootstrap--install-straight-if-required))

(defun sbw/bootstrap--pkg-name (pkg)
  (plist-get pkg :name))

(defun sbw/bootstrap--pkg-action (pkg)
  (plist-get pkg :action))

(defun sbw/bootstrap--install-if-required (pkg)
  (let* ((name   (sbw/bootstrap--pkg-name pkg))
         (action (sbw/bootstrap--pkg-action pkg)))
    (when (not (eq action :config-only))
      (eval `(straight-use-package (quote ,name))))))

(defun sbw/bootstrap--configure-if-required (pkg)
  (let* ((name        (sbw/bootstrap--pkg-name pkg))
         (config-fnam (concat  "sbw-configure-" (symbol-name name) ".el"))
         (config-dir  (concat sbw/lisp-path "/package-config"))
         (fnam        (concat config-dir "/" config-fnam)))
    (message (concat "Loading " fnam))
    (if (file-exists-p fnam)
        (load-file fnam)
      (eval `(require (quote, name))))))

(defun sbw/bootstrap-packages (pkgs)
  "Bootstrap the packages PKGS and then either configure or require the package."
  (let* ((statuses (make-hash-table :test 'eq)))
    (loop for pkg in pkgs
          do
          (puthash (symbol-name (sbw/bootstrap--pkg-name pkg)) "[    ]" statuses))
    (loop for pkg in pkgs
          do
          (sbw/bootstrap--update-package-status pkg pkgs "[ -- ]" statuses)
          (sbw/bootstrap--install-if-required pkg)
          (sbw/bootstrap--configure-if-required pkg)
          (sbw/bootstrap--update-package-status pkg pkgs "[ OK ]" statuses))
    (with-current-buffer sbw/bootstrap--buffer-name
      (end-of-line)
      (insert "\n\nDone!"))))

(defun sbw/bootstrap-require (pkgs)
  "Just require the packages PKGS."
  (loop for pkg in pkgs
        do (eval `(require (quote, pkg)))))

(provide 'sbw-bootstrap)
