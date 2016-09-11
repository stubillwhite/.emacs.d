(require 'cl)
(require 'sbw-org-utils)
(require 'sbw-time)
(require 'org)

(setq sbw/org-report-dir (concat org-directory "/reports"))

;; General reporting utilities

(defun sbw/org-review-utils--concat-summaries (summary-map f-fmt &optional f-sort)
  (-let* ( (default-sort (lambda (a b) (string< (sbw/ht-get a :heading) (sbw/ht-get b :heading))))
           (f-sort       (or f-sort default-sort)) )
    (sbw/ht-map-vals 
     (lambda (summaries)
       (-reduce-from
        (lambda (acc s)
          (concat acc (funcall f-fmt s)))
        ""
        (-sort f-sort summaries)))
     summary-map)))

(defun sbw/org-review-utils--concat-categories (summary-map f-fmt &optional f-sort)
  (-let* ( (default-sort 'string<)
           (f-sort       (or f-sort default-sort)) )
    (-reduce-from
     (lambda (acc c)
       (concat acc (funcall f-fmt c) (sbw/ht-get summary-map c) "\n"))
     ""
     (-sort f-sort (sbw/ht-keys summary-map)))))

;;
;; Report: Completed tasks
;;

(defun sbw/org-review-completed-tasks--completed-in-period? (config x)
  (let* ( (start  (sbw/ht-get config :start))
          (end    (sbw/ht-get config :end))
          (closed (sbw/ht-get x :closed)) ) 
    (and
     closed
     (time-less-p start closed)
     (time-less-p closed end)
     (equal (sbw/ht-get x :state) "DONE"))))

(defun sbw/org-review-completed-tasks--collect-by-category (summaries)
  (sbw/collect-by (lambda (y) (sbw/ht-get y :category)) summaries))

(defun sbw/org-review-completed-tasks--construct-report (summary-map)
  (concat
   (sbw/org-review-utils--concat-categories
    (sbw/org-review-utils--concat-summaries summary-map
                                            (lambda (x) (format "    - %s\n" (sbw/ht-get x :heading))))
    (lambda (x) (format "- %s\n" x)))
   "\n"))

(defun sbw/org-review-completed-tasks-generate-report (config summaries)
  (->> summaries
       (-filter (-partial 'sbw/org-review-completed-tasks--completed-in-period? config))
       (sbw/org-review-completed-tasks--collect-by-category)
       (sbw/org-review-completed-tasks--construct-report)))

;;
;; Report: Clocked time
;;

(defvar sbw/org-review-clocked-time--time-zero (seconds-to-time 0))

(defun sbw/org-review-clocked-time--sum-times (times)
  (-reduce-from 'time-add sbw/org-review-clocked-time--time-zero times))

(defun sbw/org-review-clocked-time--intersect? (t1-start t1-end t2-start t2-end)
  (-let* ( (lt-eq? (lambda (a b) (or (equal a b) (time-less-p a b)))) )
    (or
     (and (funcall lt-eq? t2-start t1-start) (funcall lt-eq? t1-start t2-end))
     (and (funcall lt-eq? t1-start t2-start) (funcall lt-eq? t2-start t1-end)))))

(defun sbw/org-review-clocked-time--intersection (t1-start t1-end t2-start t2-end)
  (if (sbw/org-review-clocked-time--intersect? t1-start t1-end t2-start t2-end)
      (-let* ( (start (sbw/time-max t1-start t2-start))
               (end   (sbw/time-min t1-end   t2-end)) )
        (time-subtract end start))
    sbw/org-review-clocked-time--time-zero))

(defun sbw/org-review-clocked-time--time-clocked-in-period (config clock-line)
  (-let* ( ((&hash :start start :end end) config)
           (re     "\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\)")
           (match  (s-match re clock-line)) )
    (if match
        (-let* ( (tstart (org-time-string-to-time (nth 1 match)))
                 (tend   (org-time-string-to-time (nth 2 match))) )
          (sbw/org-review-clocked-time--intersection start end tstart tend))
      sbw/org-review-clocked-time--time-zero)))

(defun sbw/org-review-clocked-time--total-time-clocked-in-period (config summary)
  (sbw/org-review-clocked-time--sum-times
   (-map
    (lambda (x) (sbw/org-review-clocked-time--time-clocked-in-period config x))
    (sbw/ht-get summary :clock))))

(defun sbw/org-review-clocked-time--add-total-time-clocked-in-period (config summaries)
  (-map
   (lambda (x) (sbw/ht-assoc x :clocked-in-period (sbw/org-review-clocked-time--total-time-clocked-in-period config x)))
   summaries))

(defun sbw/org-review-clocked-time--some-time-clocked-in-period? (config summary)
  (time-less-p sbw/org-review-clocked-time--time-zero (sbw/ht-get summary :clocked-in-period)))

(defun sbw/org-review-clocked-time--collect-by (f l)
  (-reduce-from    
   (lambda (acc x)
     (let* ( (k (funcall f x))
             (v (sbw/ht-get acc k (list))) )
       (sbw/ht-assoc acc k (cons x v))))
   (sbw/ht-create)
   l))

(defun sbw/org-review-clocked-time--collect-by-category (summaries)
  (sbw/org-review-clocked-time--collect-by
   (lambda (x) (sbw/ht-get x :category))
   summaries))

(defun sbw/org-review-clocked-time--map-over-vals (f summary-map)
  (lexical-let* ( (f f) )
    (sbw/ht-map-vals
     (lambda (x) (-map f x))
     summary-map)))

(defun sbw/org-review-clocked-time--add-category-totals (summary-map)
  (-let* ( (get-clocked     (lambda (x) (sbw/ht-get x :clocked-in-period)))
           (sum-category    (lambda (x) (sbw/org-review-clocked-time--sum-times (-map get-clocked x))))
           (category-totals (sbw/ht-map-vals sum-category summary-map))
           (add-total       (lambda (x) (sbw/ht-assoc x :category-total
                                                 (sbw/ht-get category-totals (sbw/ht-get x :category))))) )
    (sbw/ht-map-vals
     (lambda (summaries) (-map add-total summaries))
     summary-map)))

(defun sbw/org-review-clocked-time--format-elapsed-time (time)
  (-let* ( (t-min   (round (/ (time-to-seconds time) 60)))
           (hours   (/ t-min 60))
           (minutes (mod t-min 60)) )
    (format "%02d:%02d" hours minutes)))

(defun sbw/org-review-clocked-time--format-summary (summary)
  (-let* ( (clocked (sbw/ht-get summary :clocked-in-period))
           (total   (sbw/ht-get summary :category-total)) )
    (format "%s *[%.0f%%, %s]*"
            (sbw/ht-get summary :heading)
            (* 100 (/ (time-to-seconds clocked) (time-to-seconds total)))
            (sbw/org-review-clocked-time--format-elapsed-time clocked))))

(defun sbw/org-review-clocked-time--construct-report (summary-map)
  (concat
   (sbw/org-review-utils--concat-categories
    (sbw/org-review-utils--concat-summaries
     summary-map
     (lambda (x)   (format "    - %s\n" (sbw/org-review-clocked-time--format-summary x)))
     (lambda (x y) (not (time-less-p (sbw/ht-get x :clocked-in-period) (sbw/ht-get y :clocked-in-period)))))
    (lambda (x)   (format "- %s\n" x)))
   "\n"))

;; TODO Replace with partial
(defun sbw/org-review-clocked-time-generate-report (config summaries)
  (-let* ( (-add-total-time-clocked-in-period (lambda (x) (sbw/org-review-clocked-time--add-total-time-clocked-in-period config x)))
           (-some-time-clocked-in-period?     (lambda (x) (sbw/org-review-clocked-time--some-time-clocked-in-period? config x))) )
    (->> summaries
         (funcall -add-total-time-clocked-in-period)
         (-filter -some-time-clocked-in-period?)
         (sbw/org-review-clocked-time--collect-by-category)
         (sbw/org-review-clocked-time--add-category-totals)
         (sbw/org-review-clocked-time--construct-report))))

;;
;; Master report
;;

(defun sbw/org-review--heading-summaries (config)
  (let* ( (org-files (sbw/ht-get config :org-files)) )
    (-mapcat 'sbw/org-utils-heading-summaries-for-file org-files)))  

(defun sbw/org-review--write-report (config report)
  (let* ( (filename (sbw/ht-get config :filename)) )
    (f-mkdir (f-dirname filename))
    (with-temp-file filename (insert report))
    (message (format "Created '%s'" filename))
    (find-file filename)
    nil))

(defun sbw/org-review--markdown-header (level s)
  (let* ( (marker (s-repeat level "#")) )
    (format "%s %s %s\n\n" marker s marker)))

(defun sbw/org-review--build-report (config)
  (let* ( (summaries (sbw/org-review--heading-summaries config)) )
    (concat
     (sbw/org-review--markdown-header 1 (sbw/ht-get config :title))
     (sbw/org-review--markdown-header 2 "Completed tasks")
     (sbw/org-review-completed-tasks-generate-report config summaries)
     (sbw/org-review--markdown-header 2 "Activity")
     (sbw/org-review-clocked-time-generate-report config summaries))))

(defun sbw/org-review-generate (config)
  "Generates the report from the configuration."
  (sbw/org-review--write-report config (sbw/org-review--build-report config)))

(defun sbw/org-review--format-date (time)
  (format-time-string "%Y%m%d" time))

(defun sbw/org-review--build-filename (prefix start end)
  (format "%s/%s-%s-to-%s.md"
          sbw/org-report-dir
          prefix
          (sbw/org-review--format-date start)
          (sbw/org-review--format-date end)))

(defun sbw/org-review--build-title (descr start end)
  (format "%s for %s to %s" descr (sbw/org-review--format-date start) (sbw/org-review--format-date end)))

(defun sbw/org-review-config (title org-files start end filename)
  "Returns the configuration to generate a report wth the specified attributes."
  (sbw/ht-create
   :title     title
   :org-files org-files
   :start     start
   :end       end
   :filename  filename))

(defun sbw/org-review-config-for-weekly-report (time)
  "Returns the configuration to generate a weekly report."
  (let* ( (weekday (sbw/ht-get (sbw/time-decompose time) :weekday))
          (start   (sbw/time-adjust-by time (- (+ weekday 7))))
          (end     (sbw/time-adjust-by time (- weekday))) )
    (sbw/org-review-config
     (sbw/org-review--build-title "Weekly report" start end)
     (sbw/ht-get sbw/org-config :all-projects)
     start
     end
     (sbw/org-review--build-filename "weekly-report" start end))))

(defun sbw/org-review-config-for-monthly-report (time)
  "Returns the configuration to generate a monthly report."
  (let* ( (day        (sbw/ht-get (sbw/time-decompose time) :day))
          (prev-month (sbw/time-decompose (sbw/time-adjust-by time (- (sbw/inc day)))))
          (last-day   (calendar-last-day-of-month (sbw/ht-get prev-month :month) (sbw/ht-get prev-month :year)))
          (start      (sbw/time-compose (sbw/ht-merge prev-month (sbw/ht-create :day 1))))
          (end        (sbw/time-compose (sbw/ht-merge prev-month (sbw/ht-create :day last-day)))) )
    (sbw/org-review-config
     (sbw/org-review--build-title "Monthly report" start end)
     (sbw/ht-get sbw/org-config :all-projects)
     start
     end
     (sbw/org-review--build-filename "monthly-report" start end))))

(defun sbw/org-review-config-for-period ()
  "Returns the configuration for a period specified interactively by the user."
  (interactive)
  (let* ( (prompt-for-date (lambda (prompt) (org-read-date nil :to-time nil prompt)))
          (start           (funcall prompt-for-date "Enter start"))
          (end             (funcall prompt-for-date "Enter end")) )
    (sbw/org-review-config
     (sbw/org-review--build-title "Report" start end)
     (sbw/ht-get sbw/org-config :all-projects)
     start
     end
     (sbw/org-review--build-filename "report" start end))))

(provide 'sbw-org-review)
