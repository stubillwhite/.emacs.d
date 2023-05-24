;; Common configration

(defun sbw/is-windows? ()
  "Returns truthy if running on windows."
  (or (eq system-type 'windows-nt) (eq system-type 'cygwin)))

(defun sbw/is-linux? ()
  "Returns truthy if running on Linux."
  (eq system-type 'gnu/linux))

(defun sbw/is-darwin? ()
  "Returns truthy if running on Darwin."
  (eq system-type 'darwin))

(defvar sbw/frame-title-format "emacs [%b]")

(defun sbw/dropbox-subfolder (path)
  (let* ((dropbox-folder (cond
                          ((sbw/is-linux?)   "~/Dropbox")
                          ((sbw/is-windows?) "/cygdrive/c/Users/IBM_ADMIN/Dropbox")
                          ((sbw/is-darwin?)  "~/Library/CloudStorage/Dropbox"))))
    (s-lex-format "${dropbox-folder}/${path}")))

(provide 'sbw-common-config)
