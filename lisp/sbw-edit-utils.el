(defun sbw/edit-utils-insert-duplicate-line-above ()
  "Duplicates the current line above the current point."
  (interactive)
  (let* ( (line (thing-at-point 'line)) )
    (save-excursion
      (move-beginning-of-line nil)
      (insert line))))

(defun sbw/edit-utils-insert-duplicate-line-below ()
  "Duplicates the current line below the current point."
  (interactive)
  (sbw/edit-utils-insert-duplicate-line-above)
  (previous-line))

(provide 'sbw-edit-utils)
