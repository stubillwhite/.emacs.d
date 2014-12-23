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

(defun sbw/-org-review-generate-report (config category-summaries)
  "Returns a report of the tasks in the specified org files. The formatter-func should accept a category and a list of heading
   summaries of tasks for a particular category, and return a string containing the formatted report for that category."
  (let* ( (formatter (sbw/ht-get config :formatter))
          (sorted-keys     (-sort 'string-lessp (sbw/ht-keys category-summaries)))
          (generate-report (lambda (x) (funcall formatter config x (sbw/ht-get category-summaries x)))) )
    (apply 'concat
      (-map generate-report sorted-keys))))

(defun sbw/org-review-generate-category-report (config)
  (let* ( (org-files (sbw/ht-get config :org-files)) )
    (sbw/-org-review-generate-report config (sbw/-org-review-category-summaries org-files))))

(defun sbw/-org-review-write-report (report filename)
  "Write the report to filename and open it for review."
  (with-temp-file filename (insert report))
  (message (format "Created '%s'" filename))
  (find-file filename)
  nil)

(defun sbw/org-review (config)
  (let* ( (filename  (sbw/ht-get config :filename))
          (formatter (sbw/ht-get config :formatter))
          (org-files (sbw/ht-get config :org-files))
          (report    (sbw/org-review-generate-category-report config)))
    (sbw/-org-review-write-report report filename)))





;; Review formatter


(defun sbw/-org-review-is-completed-between? (start end x)
  "Return t if task x was completed between start and end."
  (let* ( (closed (gethash :closed x)) )
    (and
      closed
      (time-less-p start closed)
      (time-less-p closed end)
      (equal (gethash :state x) "DONE"))))

;; TODO Define a macro to destructure

(defun sbw/-org-review-report-completed-tasks (config heading-summaries)
  (let* ( (start           (sbw/ht-get config :start))
          (end             (sbw/ht-get config :end))
          (to-string       (lambda (x) (sbw/ht-get x :heading)))
          (task-comparator (lambda (x y) (string< (funcall to-string x) (funcall to-string y)))) 
          (completed-tasks (-filter (-partial 'sbw/-org-review-is-completed-between? start end) heading-summaries)) )
    (apply 'concat
      (-map
        (lambda (x) (concat " - " (funcall to-string x) "\n"))
        (-sort task-comparator completed-tasks)))))

(defun sbw/-org-review-new-formatter (config category heading-summaries)
  (let* ( (completed-report (sbw/-org-review-report-completed-tasks config heading-summaries)) )
    (when (not (string= "" completed-report))
      (concat
        (sbw/heading-two category)
        "\n"
        completed-report
        "\n"))))

(defun sbw/-org-review-default-formatter-func (config category heading-summaries)
  (sbw/-org-review-new-formatter config category heading-summaries))

;; Review configs

(defun sbw/org-review-config (org-files start end filename formatter)
  (sbw/ht-create
    :org-files org-files
    :start     start
    :end       end
    :filename  filename
    :formatter formatter))

(defun sbw/-org-review-config-weekly-review (org-files base)
  (let* ( (start    (sbw/adjust-date-by base -6))
          (end      (sbw/adjust-date-by base  1))
          (filename (sbw/report-filename start end "weekly-report")) )
    (sbw/org-review-config org-files start end filename 'sbw/-org-review-default-formatter-func)))

(defun sbw/org-review-config-previous-week (org-files)
  (sbw/-org-review-config-weekly-review org-files (apply 'encode-time (org-read-date-analyze "-sat" nil '(0 0 0)))))

(defun sbw/org-review-config-current-week (org-files)
  (sbw/-org-review-config-weekly-review org-files (apply 'encode-time (org-read-date-analyze "+sat" nil '(0 0 0)))))

(defun sbw/-org-review-config-monthly-review (org-files base)
  (let* ( (decomp-base (sbw/decompose-time base))
          (last-day    (calendar-last-day-of-month (sbw/ht-get decomp-base :month) (sbw/ht-get decomp-base :year)))
          (start       (sbw/compose-time (sbw/ht-merge decomp-base (sbw/ht-create :day 1))))
          (end         (sbw/compose-time (sbw/ht-merge decomp-base (sbw/ht-create :day last-day))))
          (filename    (sbw/report-filename start end "monthly-report")))
    (sbw/org-review-config org-files start end filename 'sbw/-org-review-default-formatter-func)))

(defun sbw/org-review-config-previous-month (org-files)
  (let* ( (decomp-curr-time (sbw/decompose-time (current-time)))
          (prev-month       (sbw/dec (sbw/ht-get decomp-curr-time  :month)))
          (base             (sbw/compose-time (sbw/ht-merge decomp-curr-time (sbw/ht-create :month prev-month)))) )
    (sbw/-org-review-config-monthly-review org-files base)))

(defun sbw/org-review-config-current-month (org-files)
  (let* ( (base (current-time)) )
    (sbw/-org-review-config-monthly-review org-files base)))

(defun sbw/org-review-weekly-report-for-previous-week (org-files)
  (sbw/-org-review-write-report org-files))

(provide 'sbw-org-review)
