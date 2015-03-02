;(define-namespace sbw/org-review-

(require 'cl)

(setq sbw/org-report-dir (concat org-directory "/reports"))

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

  ;; TODO Replace with map-categorised
  (defun -format-tasks (summaries-map)
    (-reduce-from
      (lambda (acc c)
        (sbw/ht-assoc-in acc (list c)
          (->> (sbw/ht-get summaries-map c)
            (-map (lambda (x) (format "    - %s\n" (sbw/ht-get x :heading)))))))
      summaries-map
      (sbw/ht-keys summaries-map)))
  
  (defun -construct-report (summaries-map)
    (-reduce-from
      (lambda (acc c)
        (concat
          acc
          (format "- %s\n" c)
          (apply 'concat (-sort 'string< (sbw/ht-get summaries-map c)))
          "\n"))
      ""
      (-sort 'string< (sbw/ht-keys summaries-map))))

  (defun generate-report (config summaries)
    (-let* ( (-completed-in-period? 'sbw/org-review-completed-tasks--completed-in-period?)
             (-collect-by-category  'sbw/org-review-completed-tasks--collect-by-category)
             (-format-tasks         'sbw/org-review-completed-tasks--format-tasks)
             (-construct-report     'sbw/org-review-completed-tasks--construct-report) )
      (->> summaries
        (-filter (-partial -completed-in-period? config))
        (funcall -collect-by-category)
        (funcall -format-tasks)
        (funcall -construct-report))))
  )

;;
;; Report: Project status
;;

(define-namespace sbw/org-review-project-status-

  (defun -task? (summary)
    (and
      (= 2 (sbw/ht-get summary :level))
      (not (null (sbw/ht-get summary :state)))))

  (defun -collect-by-category (summaries)
    (sbw/collect-by (lambda (y) (sbw/ht-get y :category)) summaries))

  (defun -calculate-state-counts (summaries-map)
    (let* ( (inc-count (lambda (x) (if (null x) 1 (sbw/inc x)))) )
      (sbw/ht-map-vals
        summaries-map
        (lambda (v) (-reduce-from
                 (lambda (acc x) (sbw/ht-update acc (sbw/ht-get x :state) inc-count))
                 (sbw/ht-create)
                 v)))))

  (defun -state< (a b)
    (let* ( (ordinal (sbw/ht-create
                       "TODO"      1
                       "STARTED"   2
                       "BLOCKED"   3
                       "POSTPONED" 4
                       "DONE"      5
                       "CANCELLED" 6)) )
      (< (sbw/ht-get ordinal a) (sbw/ht-get ordinal b))))

  ;; TODO replace with map categorised
  (defun -format-counts (summaries-map)
    (let* ( (-state< 'sbw/org-review-project-status--state<) )
      (sbw/ht-map-vals
        summaries-map
        (lambda (counts-map)
          (-reduce-from
            (lambda (acc c)
              (concat
                acc
                (format "    - %s %d\n" c (sbw/ht-get counts-map c))))
            ""
            (-sort -state< (sbw/ht-keys counts-map)))))))

  (defun -construct-report (summaries-map)
    (-reduce-from
      (lambda (acc c)
        (concat
          acc
          (format "- %s\n" c)
          (sbw/ht-get summaries-map c)
          "\n"))
      ""
      (-sort 'string< (sbw/ht-keys summaries-map))))
  
  (defun generate-report (config summaries)
    (-let* ( (-task?                  'sbw/org-review-project-status--task?)
             (-collect-by-category    'sbw/org-review-project-status--collect-by-category)
             (-calculate-state-counts 'sbw/org-review-project-status--calculate-state-counts)
             (-format-counts          'sbw/org-review-project-status--format-counts)
             (-construct-report       'sbw/org-review-project-status--construct-report)
             )
      (->> summaries
        (-filter -task?)
        (funcall -collect-by-category)
        (funcall -calculate-state-counts)
        (funcall -format-counts)
        (funcall -construct-report))))
  )

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

  (defun -collect-by-category (summaries)
    (sbw/collect-by (lambda (y) (sbw/ht-get y :category)) summaries))

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
      (format "[% .0f%% %s/%s] %s"
        
        
                (* 100 (/ (time-to-seconds clocked) (time-to-seconds total)))
        (-format-elapsed-time clocked)        
        (-format-elapsed-time total)
        (sbw/ht-get summary :heading)
        )))

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
          (concat acc (funcall f-fmt c) (sbw/ht-get summary-map c)))
        ""
        (-sort f-sort (sbw/ht-keys summary-map)))))

  (defun -construct-report (summary-map)
    (-let* ( (-concat-summaries  'sbw/org-review-clocked-time--concat-summaries)
             (-concat-categories 'sbw/org-review-clocked-time--concat-categories) )
      (concat
        (-concat-categories
          (-concat-summaries summary-map
            (lambda (x) (format "    - %s\n" (-format-summary x)))
            (lambda (x y) (not (time-less-p (sbw/ht-get x :clocked-in-period) (sbw/ht-get y :clocked-in-period)))))
          (lambda (x) (format "- %s\n" x)))
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

(defun sbw/-org-review-build-report (config)
  (let* ( (summaries (sbw/-org-review-heading-summaries config))
          (completed (sbw/org-review-completed-tasks-generate-report config summaries))
          (clocked   (sbw/org-review-clocked-time-generate-report config summaries))
          (project   (sbw/org-review-project-status-generate-report config summaries)) )
    (concat
      (sbw/markdown-header 1 (sbw/ht-get config :title))
      (sbw/markdown-header 2 "Completed tasks")
      completed
      (sbw/markdown-header 2 "Activity")
      clocked
      (sbw/markdown-header 2 "Project status")
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
