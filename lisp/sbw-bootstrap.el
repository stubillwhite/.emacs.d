(require 'cl-lib)

(defvar sbw/lisp-path)

(defcustom sbw/bootstrap--buffer-name "*bootstrap-process*"
  "Name of buffer used for process output."
  :type 'string)

;; Prevent the built-in package manager from customising my files
(setq custom-file (concat user-emacs-directory "emacs-customisations.el"))
(load custom-file)

(defun sbw/bootstrap-load-secrets ()
  "Load encrypted secrets."
  (interactive)
  (require 'secrets "secrets.el.gpg"))

(defun sbw/bootstrap--install-straight-if-required ()
  (let ((bootstrap-file    (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
  (cl-loop for k in pkgs
        do (with-current-buffer sbw/bootstrap--buffer-name
             (let* ((name (symbol-name (sbw/bootstrap--pkg-name k))))
               (insert (format "%40s : %s\n" name (gethash name statuses))))))
  (goto-char (point-min))
  (forward-line (+ 3 (cl-position pkg pkgs)))
  (read-only-mode -1)
  (redisplay t))

(defun sbw/bootstrap--update-package-status (pkg pkgs new-status statuses)
  (puthash (symbol-name (sbw/bootstrap--pkg-name pkg)) new-status statuses)
  (sbw/bootstrap--display-package-statuses pkg pkgs statuses))

(defun sbw/bootstrap-init ()
  "Configure bootstrap."
  (switch-to-buffer (messages-buffer))
  (split-window-right)
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
      (message (concat "Installing " (symbol-name name)))
      (eval `(straight-use-package (quote ,name))))))

(defun sbw/bootstrap--configure-if-required (pkg)
  (let* ((name        (sbw/bootstrap--pkg-name pkg))
         (config-fnam (concat  "sbw-configure-" (symbol-name name) ".el"))
         (config-dir  (concat sbw/lisp-path "/package-config"))
         (fnam        (concat config-dir "/" config-fnam)))
    (if (file-exists-p fnam)
        (load-file fnam)
      (sbw/bootstrap-require (list name)))))

(defun sbw/bootstrap--alternate-installation-flow (pkg)
  (let* ((name        (sbw/bootstrap--pkg-name pkg))
         (config-fnam (concat  "sbw-configure-" (symbol-name name) ".el"))
         (config-dir  (concat sbw/lisp-path "/package-config"))
         (fnam        (concat config-dir "/" config-fnam)))
    (if (file-exists-p fnam)
        (load-file fnam)
      (eval `(straight-use-package (quote ,name)))
      (eval `(require (quote ,name))))))

(defun sbw/bootstrap-packages (pkgs)
  "Bootstrap the packages PKGS and then either configure or require the package."
  (let* ((statuses (make-hash-table :test 'eq)))
    (cl-loop for pkg in pkgs
          do
          (puthash (symbol-name (sbw/bootstrap--pkg-name pkg)) "" statuses))
    (cl-loop for pkg in pkgs
          do
          (if (eq (sbw/bootstrap--pkg-name pkg) 'cider)
              (sbw/bootstrap--alternate-installation-flow pkg)
            (sbw/bootstrap--update-package-status pkg pkgs "Installing..." statuses)
            (sbw/bootstrap--install-if-required pkg)
            (sbw/bootstrap--update-package-status pkg pkgs "Installed" statuses)))
    (cl-loop for pkg in pkgs
          do
          (if (eq (sbw/bootstrap--pkg-name pkg) 'cider)
              (message "Skipping")
            (sbw/bootstrap--update-package-status pkg pkgs "Configuring..." statuses)
            (sbw/bootstrap--configure-if-required pkg)
            (sbw/bootstrap--update-package-status pkg pkgs "Done" statuses)))
    (with-current-buffer sbw/bootstrap--buffer-name
      (end-of-line)
      (insert "\n\nDone!"))))

(defun sbw/bootstrap-require (pkgs)
  "Just require the packages PKGS."
  (cl-loop for pkg in pkgs
        do (progn
             (message (concat "Requiring package " (symbol-name pkg)))
             (eval `(require (quote, pkg))))))

(provide 'sbw-bootstrap)
