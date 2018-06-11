(defun sbw/assq-ensure-is-first (key alist)
  "Mutates associative ALIST so that the value for KEY is first."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry)))

(defun sbw/assq-ensure-is-last (key alist)
  "Mutates associative ALIST so that the value for KEY is last."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry :append)))

(defun sbw/truncate-string (s n)
  "Returns S truncated to N characters with ellipsis if truncation occurred."
  (if (> (length s) n)
    (concat (substring s 0 (- n 1)) "\u2026")
    s))

(defun sbw/open-and-switch-to-window (buf)
  "Open the buffer if it not currently open and switch focus to it."
  (when (not (eq (current-buffer) buf))
    (if (get-buffer-window buf)
        (progn
          (switch-to-buffer-other-window buf))
      (progn
        (split-window-right)
        (switch-to-buffer-other-window (buffer-name))
        (switch-to-buffer buf)))))

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

(provide 'sbw-utils)
