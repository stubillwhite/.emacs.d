;(define-namespace sbw/org-review-

(require 'cl)

(setq sbw/org-review-config
  (sbw/ht-create
    :org-directory   org-directory
    :supercategories '("personal" "work" "non-project")))

;;
;; Org file navigation
;;

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

(defun sbw/-org-review-extract-clock ()
  "Return a list of the clock entries for the heading at point."
  (-let* [ (is-clock?   (lambda (x) (-let [(k . v) x] (string-equal k "CLOCK"))))
           (clock-value (lambda (x) (-let [(k . v) x] v))) ]
    (->> (org-entry-properties)
      (-filter is-clock?)
      (-map clock-value))))

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
      (puthash :clock    (sbw/-org-review-extract-clock) summary)
      (puthash :closed   (sbw/-org-review-extract-timestamp  (cdr (assoc "CLOSED" (org-entry-properties)))) summary) ;; TODO Replace with something more elegant
      )
    summary))

(defun sbw/org-review-heading-summaries-for-file (fnam)
  "Return summaries for all the headings in file fnam."
  (set-buffer (find-file-noselect fnam))
  (-map 'sbw/-org-review-extract-heading-summary (sbw/-org-review-heading-points)))

(defun sbw/-org-review-heading-summaries (config)
  (let* ( (org-files (sbw/ht-get config :org-files)) )
    (-mapcat 'sbw/org-review-heading-summaries-for-file org-files)))

;;
;; General reporting utilities

(defun sbw/-org-review-write-report (config report)
  "Write the report to filename and open it for review."
  (let* ( (filename (sbw/ht-get config :filename)) )
    (with-temp-file filename (insert report))
    (message (format "Created '%s'" filename))
    (find-file filename)
    nil))

(defun sbw/markdown-header (level s)
  (let* ( (marker (s-repeat level "#")) )
    (format "%s %s %s\n\n" marker s marker)))

;; TODO Break reporting and generating
(defun sbw/-org-review-faceted-report (config summaries f-facet f-report)
  (let* ( (faceted (sbw/collect-by f-facet summaries))
          (facets  (-sort 'string-lessp (sbw/ht-keys faceted))) )
    (apply 'concat
      (-map
        (lambda (facet) (funcall f-report config facet (sbw/ht-get faceted facet)))
        facets))))

(defun sbw/-org-review-facet-by-category (summary)
  (sbw/ht-get summary :category))

;;
;; Report: Completed tasks
;;

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

(defun sbw/-org-review-completed-tasks-report-format-category (category summaries)
  (let* ( (to-string       (lambda (x) (sbw/ht-get x :heading)))
          (task-comparator (lambda (x y) (string< (funcall to-string x) (funcall to-string y)))) )
    (concat
      ;(sbw/markdown-header 4 category)
      (format "- %s\n" category)
      (apply 'concat
        (-map
          (lambda (x) (concat "    - " (funcall to-string x) "\n"))
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

;;
;; Report: Task counts
;; TODO RENAME

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
      (format "- %s\n" category)
      ;(sbw/markdown-header 3 category)
      (if (sbw/ht-keys state-counts)
        (apply 'concat
          (-map
            (lambda (state) (format "    - %s %d\n" state (sbw/ht-get state-counts state)))
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

(defun sbw/-org-review-project-report-state-counts (config category summaries)
  (let* ( (state-counts (sbw/-org-review-project-report-collect-state-counts summaries)) )
    (sbw/-org-review-project-report-format-category category state-counts)))

(defun sbw/-org-review-project-report (config summaries)
  (sbw/-org-review-faceted-report
    config
    summaries
    'sbw/-org-review-facet-by-category
    'sbw/-org-review-project-report-state-counts))

;;
;; Report: Clocked time
;; The amount of time clocked per task in the configured period
;;

(define-namespace sbw/org-review-clocked-time-report-

  (defun -time-zeron ()
    (seconds-to-time 0))
  
  (defun -sum-times (times)
    (-reduce-from 'time-add (seconds-to-time 0) times))

  (defun -periods-overlap? (t1-start t1-end t2-start t2-end)
    (or
      (and (time-less-p t2-start t1-start) (time-less-p t1-start t2-end))
      (and (time-less-p t1-start t2-start) (time-less-p t2-start t1-end))))
  
  (defun -intersection (t1-start t1-end t2-start t2-end)
    (if (-periods-overlap? t1-start t1-end t2-start t2-end)
      (-let* ( (start (sbw/time-max t1-start t2-start))
               (end   (sbw/time-min t1-end   t2-end)) )
        (time-subtract end start))
      (seconds-to-time 0)))

  (defun -time-clocked-in-period (config clock-line)
    (-let* ( ((&hash :start start :end end) config)
             (re     "\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\)")
             (match  (s-match re clock-line)) )
      (if match
        (-let* ( (tstart (org-time-string-to-time (nth 1 match)))
                 (tend   (org-time-string-to-time (nth 2 match))) )
          (-intersection start end tstart tend))
        (seconds-to-time 0))))

  (defun -time-clocked-for-task-in-period (config summary)
    (-let* ( ((&hash :clock clock) summary) )
      (-sum-times
        (-map (-partial 'sbw/org-review-clocked-time-report--time-clocked-in-period config) clock))))

  (defun -clocked-tasks-in-period (config summaries)
    (let* ( (add-clocked-time   (lambda (x) (sbw/ht-assoc x :clocked-time (-time-clocked-for-task-in-period config x))))
            (clocked-in-period? (lambda (x) (time-less-p (seconds-to-time 0) (sbw/ht-get x :clocked-time)))) )
      (->> summaries
        (-map add-clocked-time)
        (-filter clocked-in-period?))))

  (defun -format-elapsed-time (time)
    (-let* ( (t-min   (round (/ (time-to-seconds time) 60)))
             (hours   (/ t-min 60))
             (minutes (mod t-min 60)) )
      (format "%02d:%02d" hours minutes)))

  


  
  (defun -format-clocked-time-by-task-report (config facet summaries)
    (-let* ( (total-time     (-sum-times (-map (lambda (x) (sbw/ht-get x :clocked-time)) summaries)))
             (format-summary (lambda (x)
                               (-let* ( ((&hash :heading heading :clocked-time clocked-time) x) )
                                 (format "    - %s [%s]\n" heading (-format-elapsed-time clocked-time))))) )
      (concat
        (format "- %s [%s]\n" facet (-format-elapsed-time total-time))
        (apply 'concat
          (-map format-summary summaries)))))

  (defun by-task (config summaries)
    (-let* ( (clocked-tasks (-clocked-tasks-in-period config summaries)) )
      (concat
        (sbw/markdown-header 3 "Time per task")
        (if clocked-tasks
          (sbw/-org-review-faceted-report
            config
            clocked-tasks
            'sbw/-org-review-facet-by-category
            'sbw/org-review-clocked-time-report--format-clocked-time-by-task-report)
          "No clocked tasks")
        "\n")))

  (defun -category-totals (config summaries)
    (-let* ( (time-zero   (seconds-to-time 0))
             (sum-clocked (lambda (a b) (time-add a (sbw/ht-get b :clocked-time)))) )
      (-> (sbw/org-review-clocked-time-report--clocked-tasks-in-period config summaries)
        ((lambda (x) (sbw/collect-by 'sbw/-org-review-facet-by-category x)))
        (sbw/ht-map-vals (lambda (x) (-reduce-from sum-clocked time-zero x))))))
  
  (defun by-category (config summaries)
    (concat
      (-let* ( (time-zero (seconds-to-time 0))
               (totals    (-category-totals config summaries))
               (total     (-reduce-from 'time-add time-zero (sbw/ht-vals totals))) )
        (concat
          (sbw/markdown-header 3 "Time per category")
          (if totals
            (apply 'concat
              (-map
                (lambda (x)
                  (format "- %s %s [%.0f%%]\n" x (-format-elapsed-time (sbw/ht-get totals x)) (* 100
                                                                                        (/ (time-to-seconds (sbw/ht-get totals x))
                                                                                          (time-to-seconds total)))))
                (sbw/ht-keys totals)))
            "No clocked tasks")
          "\n"))))

  
  )

(defun sum-time (a b)
  (time-add a (sbw/ht-get b :clocked-time)))

;;
;; Report: Other
;;












;;
;; Master report
;;

(defun sbw/-org-review-build-report (config)
  (let* ( (summaries        (sbw/-org-review-heading-summaries config))
          (completed        (sbw/-org-review-completed-tasks-report config summaries))
          (time-by-category (sbw/org-review-clocked-time-report-by-category config summaries))
          (time-by-task     (sbw/org-review-clocked-time-report-by-task config summaries))
          (project          (sbw/-org-review-project-report config summaries))
          )
    (concat
      (sbw/markdown-header 1 (sbw/ht-get config :title))
      (sbw/markdown-header 2 "Activity report")
      completed
      time-by-task
      time-by-category
      (sbw/markdown-header 2 "Project report")
      project
      )))

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
          (start   (sbw/adjust-time-by-days time (- (+ weekday 7))))
          (end     (sbw/adjust-time-by-days time (- weekday))) )
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
