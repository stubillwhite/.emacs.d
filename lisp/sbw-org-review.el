(require 'cl)

(setq sbw/org-report-dir (concat org-directory "/reports"))

(setq sbw/org-review-config
  (sbw/ht-create
    :org-directory   org-directory
    :supercategories '("personal" "work" "non-project")))

;;
;; Org file navigation
;;

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

;;
;; General reporting utilities

(define-namespace sbw/org-review-utils-

  (defun -concat-summaries (summary-map f-fmt &optional f-sort)
    (-let* ( (default-sort (lambda (a b) (string< (sbw/ht-get a :heading) (sbw/ht-get b :heading))))
             (f-sort       (or f-sort default-sort)) )
      (sbw/ht-map-vals summary-map
        (lambda (summaries)
          (-reduce-from
            (lambda (acc s)
              (concat acc (funcall f-fmt s)))
            ""
            (-sort f-sort summaries))))))

  (defun -concat-categories (summary-map f-fmt &optional f-sort)
    (-let* ( (default-sort 'string<)
             (f-sort       (or f-sort default-sort)) )
      (-reduce-from
        (lambda (acc c)
          (concat acc (funcall f-fmt c) (sbw/ht-get summary-map c) "\n"))
        ""
        (-sort f-sort (sbw/ht-keys summary-map))))))

;;
;; Report: Completed tasks
;;

(define-namespace sbw/org-review-completed-tasks-

  (defun -completed-in-period? (config x)
    (let* ( (start  (sbw/ht-get config :start))
            (end    (sbw/ht-get config :end))
            (closed (sbw/ht-get x :closed)) ) 
      (and
        closed
        (time-less-p start closed)
        (time-less-p closed end)
        (equal (sbw/ht-get x :state) "DONE"))))

  (defun -collect-by-category (summaries)
    (sbw/collect-by (lambda (y) (sbw/ht-get y :category)) summaries))

  (defun -construct-report (summary-map)
    (-let* ( (-concat-summaries  'sbw/org-review-utils--concat-summaries)
             (-concat-categories 'sbw/org-review-utils--concat-categories) )
      (concat
        (funcall -concat-categories
          (funcall -concat-summaries summary-map
            (lambda (x) (format "    - %s\n" (sbw/ht-get x :heading))))
          (lambda (x) (format "- %s\n" x)))
        "\n")))
  
  (defun generate-report (config summaries)
    (-let* ( (-completed-in-period? 'sbw/org-review-completed-tasks--completed-in-period?)
             (-collect-by-category  'sbw/org-review-completed-tasks--collect-by-category)
             (-construct-report     'sbw/org-review-completed-tasks--construct-report) )
      (->> summaries
        (-filter (-partial -completed-in-period? config))
        (funcall -collect-by-category)
        (funcall -construct-report)))))

;;
;; Report: Clocked time
;;

(define-namespace sbw/org-review-clocked-time-

  (defvar -time-zero (seconds-to-time 0))

  (defun -sum-times (times)
    (-reduce-from 'time-add -time-zero times))
  
  (defun -intersect? (t1-start t1-end t2-start t2-end)
    (-let* ( (lt-eq? (lambda (a b) (or (equal a b) (time-less-p a b)))) )
      (or
        (and (funcall lt-eq? t2-start t1-start) (funcall lt-eq? t1-start t2-end))
        (and (funcall lt-eq? t1-start t2-start) (funcall lt-eq? t2-start t1-end)))))
  
  (defun -intersection (t1-start t1-end t2-start t2-end)
    (if (-intersect? t1-start t1-end t2-start t2-end)
      (-let* ( (start (sbw/time-max t1-start t2-start))
               (end   (sbw/time-min t1-end   t2-end)) )
        (time-subtract end start))
      -time-zero))

  (defun -time-clocked-in-period (config clock-line)
    (-let* ( ((&hash :start start :end end) config)
             (re     "\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\)")
             (match  (s-match re clock-line)) )
      (if match
        (-let* ( (tstart (org-time-string-to-time (nth 1 match)))
                 (tend   (org-time-string-to-time (nth 2 match))) )
          (-intersection start end tstart tend))
        -time-zero)))

  (defun -total-time-clocked-in-period (config summary)
    (-sum-times
      (-map
        (lambda (x) (-time-clocked-in-period config x))
        (sbw/ht-get summary :clock))))

  (defun -add-total-time-clocked-in-period (config summaries)
    (-map
      (lambda (x) (sbw/ht-assoc x :clocked-in-period (-total-time-clocked-in-period config x)))
      summaries))

  (defun -some-time-clocked-in-period? (config summary)
    (time-less-p -time-zero (sbw/ht-get summary :clocked-in-period)))

  (defun -collect-by (f l)
    (-reduce-from    
      (lambda (acc x)
        (let* ( (k (funcall f x))
                (v (sbw/ht-get acc k (list))) )
          (sbw/ht-assoc acc k (cons x v))))
      (sbw/ht-create)
      l))
  
  (defun -collect-by-category (summaries)
    (-collect-by
      (lambda (x) (sbw/ht-get x :category))
      summaries))

  (defun -map-over-vals (f summary-map)
    (lexical-let* ( (f f) )
      (sbw/ht-map-vals
        summary-map
        (lambda (x) (-map f x)))))
  
  (defun -add-category-totals (summary-map)
    (-let* ( (get-clocked     (lambda (x) (sbw/ht-get x :clocked-in-period)))
             (sum-category    (lambda (x) (-sum-times (-map get-clocked x))))
             (category-totals (sbw/ht-map-vals summary-map sum-category))
             (add-total       (lambda (x) (sbw/ht-assoc x :category-total
                                       (sbw/ht-get category-totals (sbw/ht-get x :category))))) )
      (sbw/ht-map-vals
        summary-map
        (lambda (summaries) (-map add-total summaries)))))

  (defun -format-elapsed-time (time)
    (-let* ( (t-min   (round (/ (time-to-seconds time) 60)))
             (hours   (/ t-min 60))
             (minutes (mod t-min 60)) )
      (format "%02d:%02d" hours minutes)))
  
  (defun -format-summary (summary)
    (-let* ( (clocked (sbw/ht-get summary :clocked-in-period))
             (total   (sbw/ht-get summary :category-total)) )
      (format "%s *[%.0f%%, %s]*"
        (sbw/ht-get summary :heading)
        (* 100 (/ (time-to-seconds clocked) (time-to-seconds total)))
        (-format-elapsed-time clocked))))

  (defun -construct-report (summary-map)
    (-let* ( (-concat-summaries  'sbw/org-review-utils--concat-summaries)
             (-concat-categories 'sbw/org-review-utils--concat-categories) )
      (concat
        (funcall -concat-categories
          (funcall -concat-summaries summary-map
            (lambda (x)   (format "    - %s\n" (-format-summary x)))
            (lambda (x y) (not (time-less-p (sbw/ht-get x :clocked-in-period) (sbw/ht-get y :clocked-in-period)))))
          (lambda (x)   (format "- %s\n" x)))
        "\n")))
  
  (defun generate-report (config summaries)
    (-let* ( (-add-total-time-clocked-in-period (lambda (x) (-add-total-time-clocked-in-period config x)))
             (-some-time-clocked-in-period?     (lambda (x) (-some-time-clocked-in-period? config x)))
             (-collect-by-category              'sbw/org-review-clocked-time--collect-by-category)
             (-add-category-totals              'sbw/org-review-clocked-time--add-category-totals)
             (-construct-report                 'sbw/org-review-clocked-time--construct-report) )
      (->> summaries
        (funcall -add-total-time-clocked-in-period)
        (-filter -some-time-clocked-in-period?)
        (funcall -collect-by-category)
        (funcall -add-category-totals)
        (funcall -construct-report))))
  )

;;
;; Master report
;;

(define-namespace sbw/org-review-
  
  (defun -heading-summaries (config)
    (let* ( (org-files (sbw/ht-get config :org-files)) )
      (-mapcat 'sbw/org-utils-heading-summaries-for-file org-files)))  

  (defun -write-report (config report)
    (let* ( (filename (sbw/ht-get config :filename)) )
      (with-temp-file filename (insert report))
      (message (format "Created '%s'" filename))
      (find-file filename)
      nil))

  (defun -markdown-header (level s)
    (let* ( (marker (s-repeat level "#")) )
      (format "%s %s %s\n\n" marker s marker)))
  
  (defun -build-report (config)
    (let* ( (summaries (-heading-summaries config))
            (completed (sbw/org-review-completed-tasks-generate-report config summaries))
            (clocked   (sbw/org-review-clocked-time-generate-report config summaries)) )
      (concat
        (-markdown-header 1 (sbw/ht-get config :title))
        (-markdown-header 2 "Completed tasks")
        completed
        (-markdown-header 2 "Activity")
        clocked)))

  (defun generate (config)
    "Generates the report from the configuration."
    (-write-report config (-build-report config)))
    
  (defun -format-date (time)
    (format-time-string "%Y%m%d" time))
  
  (defun -build-filename (prefix start end)
    (format "%s/%s-%s-to-%s.md"
      sbw/org-report-dir
      prefix
      (-format-date start)
      (-format-date end)))

  (defun -build-title (descr start end)
    (format "%s for %s to %s" descr (-format-date start) (-format-date end)))

  (defun config (title org-files start end filename)
    "Returns the configuration to generate a report wth the specified attributes."
    (sbw/ht-create
      :title     title
      :org-files org-files
      :start     start
      :end       end
      :filename  filename))

  (defun config-for-weekly-report (time)
    "Returns the configuration to generate a weekly report."
    (let* ( (weekday (sbw/ht-get (sbw/time-decompose time) :weekday))
            (start   (sbw/time-adjust-by time (- (+ weekday 7))))
            (end     (sbw/time-adjust-by time (- weekday))) )
      (config
        (-build-title "Weekly report" start end)
        sbw/org-all-files
        start
        end
        (-build-filename "weekly-report" start end))))

  (defun config-for-monthly-report (time)
    "Returns the configuration to generate a monthly report."
    (let* ( (day        (sbw/ht-get (sbw/time-decompose time) :day))
            (prev-month (sbw/time-decompose (sbw/time-adjust-by time (- (sbw/inc day)))))
            (last-day   (calendar-last-day-of-month (sbw/ht-get prev-month :month) (sbw/ht-get prev-month :year)))
            (start      (sbw/time-compose (sbw/ht-merge prev-month (sbw/ht-create :day 1))))
            (end        (sbw/time-compose (sbw/ht-merge prev-month (sbw/ht-create :day last-day)))) )
      (config
        (-build-title "Monthly report" start end)
        sbw/org-all-files
        start
        end
        (-build-filename "monthly-report" start end))))

  (defun config-for-period ()
    "Returns the configuration for a period specified interactively by the user."
    (interactive)
    (let* ( (prompt-for-date (lambda (prompt) (org-read-date nil :to-time nil prompt)))
            (start           (funcall prompt-for-date "Enter start"))
            (end             (funcall prompt-for-date "Enter end")) )
      (config
        (-build-title "Report" start end)
        sbw/org-all-files
        start
        end
        (-build-filename "report" start end)))
    ))

(provide 'sbw-org-review)
