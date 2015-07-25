(define-namespace sbw/org-utils-

  (defun -heading-points-for-current-buffer ()
    (let ((points nil))
      (save-excursion
        (show-all)
        (end-of-buffer)
        (setq points
          (-unfold
            (lambda (x) (when (outline-previous-heading) (cons (point) x)))
            :unused-seed))
        (org-overview))
      points))
  
  (defun -replace-urls-with-descriptions (s)
    (let* ( (str s) )
      (while (string-match org-bracket-link-regexp str)
        (let* ( (url   (match-string 1 str))
                (descr (match-string 3 str))
                (repl  (if descr descr url)) )
          (setq str
            (replace-regexp-in-string (regexp-quote (match-string 0 str)) repl str))))
      str))

  (defun -extract-string (x)
    (when x
      (-replace-urls-with-descriptions (substring-no-properties x))))

  (defun -extract-timestamp (x)
    (when x
      (date-to-time x)))

  (defun -extract-clock ()
    (-let* [ (is-clock?   (lambda (x) (-let [(k . v) x] (string-equal k "CLOCK"))))
             (clock-value (lambda (x) (-let [(k . v) x] v))) ]
      (->> (org-entry-properties)
        (-filter is-clock?)
        (-map clock-value))))

  (defun -extract-heading-summary (x)
    "Return a summary of the org heading at point x."
    (let* ((summary (sbw/ht-create)))
      (save-excursion
        (goto-char x)
        (puthash :filename (buffer-file-name) summary)
        (puthash :point    x summary)
        (puthash :category (-extract-string (org-entry-get-with-inheritance "CATEGORY")) summary)
        (puthash :state    (-extract-string (org-get-todo-state)) summary)
        (puthash :tags     (-extract-string (org-get-tags-at)) summary)
        (puthash :heading  (-extract-string (org-get-heading nil t)) summary)
        (puthash :level    (funcall outline-level) summary)
        (puthash :clock    (-extract-clock) summary)
        (puthash :closed   (-extract-timestamp (cdr (assoc "CLOSED" (org-entry-properties)))) summary)
        )
      summary))

  (defun heading-summaries-for-file (fnam)
    "Return summaries for all the headings in file fnam."
    (-let* ( (extract-heading-summary 'sbw/org-utils--extract-heading-summary) )
      (set-buffer (find-file-noselect fnam))
      (-map extract-heading-summary (-heading-points-for-current-buffer)))))

(provide 'sbw-org-utils)
