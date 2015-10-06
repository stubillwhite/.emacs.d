;;; sbw/text.el --- Text manipulation functions

;; See http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion

(defun sbw/text-duplicate-above ()
  "Duplicate the selection or the current line above the current point."
  )

(defun sbw/text-duplicate-below ()
  "Duplicate the selection or the current line below the current point."
  )

(defun sbw/text-insert-duplicate-line-above ()
  "Duplicates the current line above the current point."
  (interactive)
  (let* ( (line (thing-at-point 'line)) )
    (save-excursion
      (move-beginning-of-line nil)
      (insert line))))

(defun sbw/text-insert-duplicate-line-below ()
  "Duplicates the current line below the current point."
  (interactive)
  (sbw/text-insert-duplicate-line-above)
  (previous-line))

(provide 'sbw-text)
