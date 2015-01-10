;(define-namespace sbw/org-review-

(require 'cl)

(setq sbw/org-review-config
  (sbw/ht-create
    :org-directory   org-directory
    :supercategories '("personal" "work" "non-project")))

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

(defun sbw/org-review-heading-summaries-for-file (fnam)
  "Return summaries for all the headings in file fnam."
  (set-buffer (find-file-noselect fnam))
  (-map 'sbw/-org-review-extract-heading-summary (sbw/-org-review-heading-points)))

(defun sbw/-org-review-heading-summaries (config)
  (let* ( (org-files (sbw/ht-get config :org-files)) )
    (-mapcat 'sbw/org-review-heading-summaries-for-file org-files)))

(defun sbw/-org-review-completed-in-period-filter (config)
  (lexical-let* ( (start (sbw/ht-get config :start))
                  (end   (sbw/ht-get config :end)) )
    (lambda (x)
      (let* ( (closed (gethash :closed x)) )
        (and
          closed
          (time-less-p start closed)
          (time-less-p closed end)
          (equal (gethash :state x) "DONE"))))))

(defun sbw/markdown-header (level s)
  (let* ( (marker (s-repeat level "#")) )
    (format "%s %s %s\n\n" marker s marker)))

(defun sbw/-org-review-completed-tasks-report-format-category (category summaries)
  (let* ( (to-string       (lambda (x) (sbw/ht-get x :heading)))
          (task-comparator (lambda (x y) (string< (funcall to-string x) (funcall to-string y)))) )
    (concat
      (sbw/markdown-header 4 category)
      (apply 'concat
        (-map
          (lambda (x) (concat " - " (funcall to-string x) "\n"))
          (-sort task-comparator summaries)))
      "\n")))

(defun sbw/-org-review-completed-tasks-report (config summaries)
  (let* ( (completed   (-filter (sbw/-org-review-completed-in-period-filter config) summaries))
          (categorised (sbw/collect-by (lambda (y) (gethash :category y)) completed))
          (categories  (-sort 'string-lessp (sbw/ht-keys categorised))) )
    (apply 'concat
      (sbw/markdown-header 3 "Completed tasks")
      (-map
        (lambda (x) (sbw/-org-review-completed-tasks-report-format-category x (sbw/ht-get categorised x)))
        categories))))

(defun sbw/-org-review-write-report (config report)
  "Write the report to filename and open it for review."
  (let* ( (filename (sbw/ht-get config :filename)) )
    (with-temp-file filename (insert report))
    (message (format "Created '%s'" filename))
    (find-file filename)
    nil))

(defun sbw/-org-review-state-less-than (a b)
  (let* ( (ordinal (sbw/ht-create
                     "TODO"      1
                     "STARTED"   2
                     "BLOCKED"   3
                     "POSTPONED" 4
                     "DONE"      5
                     "CANCELLED" 6)) )
    (< (sbw/ht-get ordinal a) (sbw/ht-get ordinal b))))

(defun sbw/-org-review-project-report-format-category (category state-counts)
  (let* ( (states (-sort 'sbw/-org-review-state-less-than (sbw/ht-keys state-counts)))) 
    (concat
      (sbw/markdown-header 3 category)
      (if (sbw/ht-keys state-counts)
        (apply 'concat
          (-map
            (lambda (state) (format " - %s %d\n" state (sbw/ht-get state-counts state)))
            states))
        "No tasks\n")
      "\n")))

(defun sbw/-org-review-is-task? (heading-summary)
  (and
    (= 2 (sbw/ht-get heading-summary :level))
    (not (null (sbw/ht-get heading-summary :state)))))

(defun sbw/-org-review-project-report-collect-state-counts (summaries)
  (let* ( (tasks     (-filter 'sbw/-org-review-is-task? summaries))
          (inc-count (lambda (x) (if (null x) 1 (sbw/inc x)))) )
    (-reduce-from
      (lambda (acc x) (sbw/ht-update acc (sbw/ht-get x :state) inc-count))
      (sbw/ht-create)
      tasks)))

(defun sbw/-org-review-project-report-state-counts (category summaries)
  (let* ( (state-counts (sbw/-org-review-project-report-collect-state-counts summaries)) )
    (sbw/-org-review-project-report-format-category category state-counts)))

(defun sbw/-org-review-project-report (config summaries)
  (let* ( (categorised (sbw/collect-by (lambda (y) (gethash :category y)) summaries))
          (categories  (-sort 'string-lessp (sbw/ht-keys categorised))) )
    (apply 'concat
      (-map
        (lambda (category) (sbw/-org-review-project-report-state-counts category (sbw/ht-get categorised category)))
        categories))))

(defun sbw/-org-review-build-report (config)
  (let* ( (summaries (sbw/-org-review-heading-summaries config))
          (completed (sbw/-org-review-completed-tasks-report config summaries))
          (project   (sbw/-org-review-project-report config summaries)) )
    (concat
      (sbw/markdown-header 1 (sbw/ht-get config :title))
      (sbw/markdown-header 2 "Activity report")
      completed
      (sbw/markdown-header 2 "Project report")
      project)))

(defun sbw/org-review (config)
  (sbw/-org-review-write-report config (sbw/-org-review-build-report config)))








(defun sbw/org-review-config (title org-files start end filename)
  (sbw/ht-create
    :title     title
    :org-files org-files
    :start     start
    :end       end
    :filename  filename))

(defun sbw/-org-review-filename (prefix start end)
  "Return the filename for a report in standard form."
  (let* ( (format-date (-partial 'format-time-string "%Y%m%d")) )
    (format "%s/%s-%s-to-%s.md"
      sbw/org-report-dir
      prefix
      (funcall format-date start)
      (funcall format-date end))))

(defun sbw/-org-review-title (descr start end)
  (let* ( (format-date (-partial 'format-time-string "%Y%m%d")) )
    (format "%s for %s to %s" descr (funcall format-date start) (funcall format-date end))))

(defun sbw/-org-review-config-weekly-report (time)
  (let* ( (weekday (sbw/ht-get (sbw/decompose-time time) :weekday))
          (start   (sbw/adjust-time-by-days time (- weekday)))
          (end     (sbw/adjust-time-by-days time (- 6 weekday))) )
    (sbw/org-review-config
      (sbw/-org-review-title "Weekly report" start end)
      sbw/org-all-files
      start
      end
      (sbw/-org-review-filename "weekly-report" start end))))

(defun sbw/-org-review-config-monthly-report (time)
  (let* ( (day        (sbw/ht-get (sbw/decompose-time time) :day))
          (prev-month (sbw/decompose-time (sbw/adjust-time-by-days time (- (sbw/inc day)))))
          (last-day   (calendar-last-day-of-month (sbw/ht-get prev-month :month) (sbw/ht-get prev-month :year)))
          (start      (sbw/compose-time (sbw/ht-merge prev-month (sbw/ht-create :day 1))))
          (end        (sbw/compose-time (sbw/ht-merge prev-month (sbw/ht-create :day last-day)))) )
    (sbw/org-review-config
      (sbw/-org-review-title "Monthly report" start end)
      sbw/org-all-files
      start
      end
      (sbw/-org-review-filename "monthly-report" start end))))

;; TODO Should prompt for input if not provided
(defun sbw/org-review-new-org-file (supercategory category)
  (let* ( (content (f-read-text (s-lex-format "${sbw/lisp-path}/sbw-org-review-new-file-template.org")))
          (path    (s-lex-format "${org-directory}/current/${supercategory}/${category}.org" )))
    (f-write
      (->> content
        (s-replace-all `(("${category}" . ,category))))
      'utf-8
      path)
    (sbw/org-find-org-files)
    (message "Created and added %s" path)))

(provide 'sbw-org-review)
