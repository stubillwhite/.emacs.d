;(define-namespace sbw/org-review-

  (defun sbw/-org-review-heading-points ()
    "Return a list of the points of all the headings in the current org buffer."
    (let ((points nil))
      (save-excursion
        (show-all)
        (end-of-buffer)
        (setq points
          (-unfold
            (lambda (x)
              (when (outline-previous-heading)
                (cons (point) x)))
            :unused-seed))
        (org-overview))
      points))
  
  (defun sbw/-org-review-strip-urls (s)
    "Return string s with any URLs replaced with their descriptions."
    (let* ( (str s) )
      (while (string-match org-bracket-link-regexp str)
        (let* ( (url   (match-string 1 str))
                (descr (match-string 3 str))
                (repl  (if descr descr url)) )
          (setq str
            (replace-regexp-in-string (regexp-quote (match-string 0 str)) repl str))))
      str))

  (defun sbw/-org-review-extract-string (x)
    "Return a string extracted from the org property."
    (when x
      (-> x
        (substring-no-properties)
        (sbw/-org-review-strip-urls))))

  (defun sbw/-org-review-extract-timestamp (x)
    "Return a timestamp extracted from the org property."
    (when x
      (date-to-time x)))

  (defun sbw/-org-review-extract-heading-summary (x)
    "Return a summary of the org heading at point x."
    (let* ((summary (sbw/ht-create)))
      (save-excursion
        (goto-char x)
        (puthash :filename (buffer-file-name) summary)
        (puthash :point    x summary)
        (puthash :category (sbw/-org-review-extract-string (org-entry-get-with-inheritance "CATEGORY")) summary)
        (puthash :state    (sbw/-org-review-extract-string (org-get-todo-state)) summary)
        (puthash :tags     (sbw/-org-review-extract-string (org-get-tags-at)) summary)
        (puthash :heading  (sbw/-org-review-extract-string (org-get-heading nil t)) summary)
        (puthash :level    (funcall outline-level) summary)
        (puthash :closed   (sbw/-org-review-extract-timestamp  (cdr (assoc "CLOSED" (org-entry-properties)))) summary))
      summary))

  (defun sbw/org-review-heading-summaries (fnam)
    "Return summaries for all the headings in file fnam."
    (set-buffer (find-file-noselect fnam))
    (-map 'sbw/-org-review-extract-heading-summary (sbw/-org-review-heading-points)))

  (defun sbw/-org-review-category-summaries (org-files)
    "Returns a hash table of category to list of heading summaries for the tasks in the specified org files."
    (let* ( (extract-heading-summaries (lambda (x) (-mapcat 'sbw/org-review-heading-summaries x)))
            (group-by-category         (lambda (x) (sbw/collect-by (lambda (y) (gethash :category y)) x))) )
      (->> org-files
        (funcall extract-heading-summaries)
        (funcall group-by-category))))

(defun sbw/-org-review-generate-report (category-summaries formatter-func)
  "Returns a report of the tasks in the specified org files. The formatter-func should accept a category and a list of heading
   summaries of tasks for a particular category, and return a string containing the formatted report for that category."
  (let* ( (sorted-keys     (-sort 'string-lessp (sbw/ht-keys category-summaries)))
          (generate-report (lambda (x) (funcall formatter-func x (sbw/ht-get category-summaries x)))) )
    (apply 'concat
      (-map generate-report sorted-keys))))

(defun sbw/org-review-generate-category-report (org-files formatter-func)
    "Returns a report of the tasks in the specified org files. The formatter-func should accept a list of heading
     summaries of tasks for a particular category, and return a string containing the formatted report for that category."
    (sbw/-org-review-generate-report (sbw/-org-review-category-summaries org-files) formatter-func))

(defun sbw/-org-review-default-formatter-func (category sheading-summaries)
  (let* ( (to-string       (lambda (x) (sbw/ht-get x :heading)))
          (task-comparator (lambda (x y) (string< (funcall to-string x) (funcall to-string y)))) )
    (concat
      (sbw/heading-two category)
      "\n"
      (apply 'concat
        (-map
          (lambda (x) (concat " - " (funcall to-string x)  "\n"))
          (-sort task-comparator sheading-summaries)))
      "\n")))

                                        ;)
;(message " ")
;(message "%s" (sbw/org-review-generate-category-report sbw/org-all-files 'sbw/-org-review-default-formatter-func))

(provide 'sbw-org-review)
