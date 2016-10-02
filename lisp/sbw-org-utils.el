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

(defun sbw/org-utils--entry-text-at-point (x)
  (save-excursion
    (goto-char x)
    (buffer-substring-no-properties (org-entry-beginning-position) (org-entry-end-position))))

(defconst sbw/org-utils--org-clock-line-re
  (let* ( (re-datetime "\\(\\[.*?\\]\\)")
          (re-elapsed  "\\([0-9]+:[0-9]\\)") )
    (format "%s %s--%s[ \t]+=>[ \t]+%s" org-clock-string
                      re-datetime
                      re-datetime
                      re-elapsed))
  "Matches a line of clocked org-mode time.")

(defun sbw/org-utils--clock-table-entries ()
  (->> (sbw/org-utils--entry-text-at-point (point))
       (s-split "\n")
       (-filter (lambda (x) (s-match sbw/org-utils--org-clock-line-re x)))))

(defun sbw/org-utils--extract-clock-table-entry (s)
  (-let* ( (match (s-match sbw/org-utils--org-clock-line-re s)) )
    (when match
      (format "%s--%s" (nth 1 match) (nth 2 match)))))

(defun sbw/org-utils--extract-clock ()
  (->> (sbw/org-utils--clock-table-entries)
       (-map 'sbw/org-utils--extract-clock-table-entry)))

(defun sbw/org-utils-heading-summary-at-point (x)
  "Return a summary of the org heading at point X."
  (let* ((summary (sbw/ht-create)))
    (save-excursion
      (goto-char x)
      (puthash :filename (buffer-file-name) summary)
      (puthash :point    x summary)
      (puthash :category (sbw/org-utils--extract-string (org-entry-get-with-inheritance "CATEGORY")) summary)
      (puthash :state    (sbw/org-utils--extract-string (org-get-todo-state)) summary)
      (puthash :tags     (sbw/org-utils--extract-string (org-get-tags-at)) summary)
      (puthash :heading  (sbw/org-utils--extract-string (org-get-heading nil t)) summary)
      (puthash :level    (funcall outline-level) summary)
      (puthash :clock    (sbw/org-utils--extract-clock) summary)
      (puthash :closed   (sbw/org-utils--extract-timestamp (cdr (assoc "CLOSED" (org-entry-properties)))) summary))
    summary)
  )

(defun sbw/org-utils-heading-summaries-for-file (fnam)
  "Return summaries for all the headings in file fnam."
  (set-buffer (find-file-noselect fnam))
  (-map 'sbw/org-utils-heading-summary-at-point (sbw/org-utils-heading-points-for-current-buffer)))

(provide 'sbw-org-utils)

