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
             ;; (insert (format "%-30s => %s\n" (symbol-name k) (gethash k statuses)))
             (insert (format "%s %s\n" (gethash k statuses) (symbol-name k)))))
  (goto-line (+ 4 (cl-position pkg pkgs)))
  (redisplay t))

(defun sbw/bootstrap--update-package-status (pkg pkgs new-status statuses)
  (puthash pkg new-status statuses)
  (sbw/bootstrap--display-package-statuses pkg pkgs statuses))

(defun sbw/bootstrap-init ()
  "Configure bootstrap."
  (sbw/bootstrap--install-straight-if-required))

(defun sbw/bootstrap--configure (pkg)
  (let* ( (pkg-name    (symbol-name pkg))
          (config-fnam (concat  "sbw-configure-" pkg-name ".el"))
          (config-dir  (concat sbw/lisp-path "/package-config"))
          (fnam        (concat config-dir "/" config-fnam)) )
    (message fnam)
    (if (file-exists-p fnam)
        (load-file fnam)
      (eval `(require (quote, pkg))))))

(defun sbw/bootstrap-packages (pkgs)
  "Bootstrap the packages PKGS and then either configure or require the package."
  (let* ((statuses (make-hash-table :test 'eq)))
    (loop for pkg in pkgs
          do (puthash pkg "[    ]" statuses))
    (loop for pkg in pkgs
          do
          (sbw/bootstrap--update-package-status pkg pkgs "[ -- ]" statuses)
          (eval `(straight-use-package (quote ,pkg)))
          (sbw/bootstrap--configure pkg)
          (sbw/bootstrap--update-package-status pkg pkgs "[ OK ]" statuses))
    (with-current-buffer sbw/bootstrap--buffer-name
      (end-of-line)
      (insert "\n\nDone!"))))

(defun sbw/bootstrap-require (pkgs)
  "Just require the packages PKGS."
  (loop for pkg in pkgs
        do (eval `(require (quote, pkg)))))

(provide 'sbw-bootstrap)
