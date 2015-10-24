;;; sbw/org-utils.el --- Navigating and interrogating org-mode headings programatically

(require 'org)

(defun sbw/org-utils-heading-points-for-current-buffer ()
  "Return all the points of the headings in the current org-mode buffer."
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

(defun sbw/org-utils--replace-urls-with-descriptions (s)
  (let* ( (str s) )
    (while (string-match org-bracket-link-regexp str)
      (let* ( (url   (match-string 1 str))
              (descr (match-string 3 str))
              (repl  (if descr descr url)) )
        (setq str
              (replace-regexp-in-string (regexp-quote (match-string 0 str)) repl str))))
    str))

(defun sbw/org-utils--extract-string (x)
  (when x
    (sbw/org-utils--replace-urls-with-descriptions (substring-no-properties x))))

(defun sbw/org-utils--extract-timestamp (x)
  (when x
    (date-to-time x)))

(defun sbw/org-utils--extract-clock ()
  (-let* [ (is-clock?   (lambda (x) (-let [(k . v) x] (string-equal k "CLOCK"))))
           (clock-value (lambda (x) (-let [(k . v) x] v))) ]
    (->> (org-entry-properties)
         (-filter is-clock?)
         (-map clock-value))))

(defun sbw/org-utils-heading-summaries-for-file (fnam)
  "Return summaries for all the headings in file fnam."
  (set-buffer (find-file-noselect fnam))
  (-map 'sbw/org-utils-heading-summary-at-point (sbw/org-utils-heading-points-for-current-buffer)))

(defun sbw/org-utils-heading-summary-at-point (x)
  "Return a summary of the org heading at point X."
  (let* ((summary (sbw/ht-create)))
    (save-excursion
      (goto-char x)
      (puthash :filename (buffer-file-name) summary)
      (puthash :point    x summary)
      (puthash :project  (sbw/org-utils--extract-string (org-entry-get-with-inheritance "PROJECT")) summary)
      (puthash :category (sbw/org-utils--extract-string (org-entry-get-with-inheritance "CATEGORY")) summary)
      (puthash :state    (sbw/org-utils--extract-string (org-get-todo-state)) summary)
      (puthash :tags     (sbw/org-utils--extract-string (org-get-tags-at)) summary)
      (puthash :heading  (sbw/org-utils--extract-string (org-get-heading nil t)) summary)
      (puthash :level    (funcall outline-level) summary)
      (puthash :clock    (sbw/org-utils--extract-clock) summary)
      (puthash :closed   (sbw/org-utils--extract-timestamp (cdr (assoc "CLOSED" (org-entry-properties)))) summary)
      )
    summary)
  )

(provide 'sbw-org-utils)
