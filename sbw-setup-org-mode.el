(require 'org-clock)

;; Default to clean view with no leading asterisks for indentation
(setq-default org-startup-indented t)

;; Org files

(setq org-directory 
      "c:/users/ibm_admin/my_local_stuff/home/my_stuff/srcs/org/")

(defun sbw/org-file (fnam)
  (concat org-directory fnam))

(defun sbw/org-files (&rest args)
  (mapcar 'sbw/org-file args))

(defconst sbw/personal-files
  (sbw/org-files "personal/finance.org" "personal/clojure.org" "personal/reading.org" "personal/technology.org" "personal/architecture.org" "personal/car.org" "personal/health.org" "personal/music.org" "personal/emacs.org" "personal/theatre.org" "personal/social.org"))

(defconst sbw/calendar-files
  (sbw/org-files "calendar/google-calendar.org"))

(defconst sbw/work-files
  (sbw/org-files "timesheet.org" "work/ibm.org" "work/apollo.org" "work/cyclops.org" "work/daedalus.org" "work/project-x.org"))

(defconst sbw/all-org-files
  (append sbw/personal-files sbw/work-files sbw/calendar-files (list)))

(setq org-agenda-files sbw/all-org-files)

(defconst sbw/org-refile-targets
  (-filter (lambda (x) (not (-contains? sbw/calendar-files x))) sbw/all-org-files))

(setq org-refile-targets
  (quote ((sbw/org-refile-targets :maxlevel . 1))))

(setq org-default-notes-file (sbw/org-file "incoming.org"))

;; General settings

(setq 
  org-clock-into-drawer         t           ;; Clock into drawers
  org-ellipsis                  "\u2026"    ;; Small ellipsis character
  org-agenda-fontify-priorities nil         ;; Don't let priority change task representation
  org-indent-mode               t           ;; Use indent mode
  org-log-into-drawer           t           ;; Log into drawers
  org-log-done                  'time       ;; Timestamp task completion so it can be used in reports
  org-M-RET-may-split-line      nil         ;; Don't split lines
  org-return-follows-link       t           ;; Easy link navigation
  org-use-property-inheritance  t           ;; Child items should inherit all from parents
  org-default-priority          ?B          ;; Default priority for unprioritised items
  appt-display-interval         5           ;; Reminder for an appointment every five minutes...
  appt-message-warning-time     15          ;; ...starting fifteeen minutes before it is due
)

;; Don't use any tags yet. This needs some attention
(setq org-tag-alist nil)

(setq org-todo-keywords
  '("TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE(d!)" "CANCELLED(c)" "POSTPONED(p)"))

(setq org-drawers
  '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "NOTES"))

(setq org-archive-save-context-info
  '(time file ltags itags todo category olpath))

(setq org-archive-location "%s_archive::")

;; Clocking
;; Clock into a task should switch state to started if it is still in a stalled state

(defun sbw/clock-in-switch-to-started (state) 
  "When clocking in, switch the task state STARTED if it is still in a stalled state." 
  (if (and (or (string-equal state "TODO")
               (string-equal state "BLOCKED")
               (string-equal state "POSTPONED"))
           (not (string-equal (buffer-name) "*Remember*"))) 
    "STARTED" 
    nil))

(setq org-clock-in-switch-to-state (quote sbw/clock-in-switch-to-started))

;; Pull in org
;; Doing this earlier seems to result in some of the above settings being lost
;; TODO Investigate what's going on there

(require 'org)

;; Link type for opening a file in a running Eclipse instance

(org-add-link-type "eclipse"
  (lambda (path)
    (start-process "Eclipse" nil "c:\\Program Files\\DevComponents\\Eclipse\\eclipse.exe" "--launcher.openFile" path)))

;; Link type for opening a file in Vim

(org-add-link-type "vim"
  (lambda (path)
    (start-process "Vim" nil "gvim.exe" path)))
     
;; Tag alignment
;; Ensure new tags are created right-aligned based on the window size, and
;; provide a handy function to re-align all tags in the buffer

(defun sbw/set-org-tags-column-based-on-window-size ()
  "Set org-tags-column to right-align based on window size. Assumes that org-ellipsis is a string."
  (setq org-tags-column (- (- (window-width) (length org-ellipsis)))))

(defun sbw/right-align-tags ()
  "Right-align the all tags in the buffer."
  (interactive)
  (sbw/set-org-tags-column-based-on-window-size)
  (org-align-all-tags)
  (redisplay t))

; TODO Sort this out
(defun org-sort-list-by-checkbox-type-1 ()
  (if (looking-at org-list-full-item-re)
    (cdr (assoc (match-string 3)
           '(("[X]" . 1) ("[-]" . 2) ("[ ]" . 3) (nil . 4))))
    4))


;; Custom agendas
;; TODO Remove boilerplate
;; TODO Look at http://stackoverflow.com/questions/22394394/orgmode-a-report-of-tasks-that-are-done-within-the-week for how to tidy this up

(defun sbw/make-title-string (title)
  (concat "\n" title "\n" (make-string (length title) ?-) "\n"))

(setq org-agenda-prefix-format
  '( (agenda . " %i %-15:c%?-15t% s")
     (timeline . "  % s")
     (todo . " %i %-15:c")
     (tags . " %i %-15:c")
     (search . " %i %-15:c")) )

(setq org-agenda-remove-tags 1)
(setq org-agenda-custom-commands
  '(
     ("c" . "Custom agenda")
     ("cw" "Work agenda"
       ( (agenda ""
           ( (org-agenda-ndays 7)
             (org-agenda-files sbw/work-files)
             ))
                  
         (tags-todo "+PRIORITY=\"A\""
           ( (org-agenda-overriding-header (sbw/make-title-string "High priority tasks"))
             (org-agenda-files sbw/work-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
             (org-agenda-files sbw/work-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "+PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
             (org-agenda-files sbw/work-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))
         ))
     
     ("cp" "Personal agenda"
       ( (agenda ""
           ( (org-agenda-ndays 7)
             (org-agenda-files sbw/personal-files)
             ))
         
         (tags-todo "+PRIORITY=\"A\""
           ( (org-agenda-overriding-header (sbw/make-title-string "High priority tasks"))
             (org-agenda-files sbw/personal-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
             (org-agenda-files sbw/personal-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "+PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
             (org-agenda-files sbw/personal-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))
         ))
     ))

;; Appointments
;; Refresh at startup and when the agenda is displayed

(defun sbw/org-refresh-appointments-from-agenda ()
  "Update the appointment list from the agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(add-hook 'org-finalize-agenda-hook 'sbw/org-refresh-appointments-from-agenda 'append)
(sbw/org-refresh-appointments-from-agenda)

(appt-activate t)

;; org-protocol

(require 'org-protocol)

(setq org-protocol-default-template-key nil)

(setq org-capture-templates
 '( ("t" "Todo" entry (file+headline (sbw/org-file "incoming.org") "Tasks")
        "* TODO %?%a\n%i")
    ("l" "Link" entry (file+olp (sbw/org-file "incoming.org") "Links")
        "* TODO %?%a\n%i")
    ("j" "Jira task" entry (file+headline (sbw/org-file "timesheet.org") "Project-X")
        "* TODO [#A] %?%a")
   ))

;; org-protocol experimental, not working

(defun sbw/eclipse-link (data)
  "Link to open a file in Eclipse."
  (message data)
  nil)

(add-to-list 'org-protocol-protocol-alist
  '("eclipse-link"
     :protocol "eclipse-link"
     :function sbw/eclipse-link))

(add-to-list 'org-protocol-protocol-alist
  '("white-test"
     :protocol "white-test"
     :function sbw/white-test))

(defun sbw/white-test (data)
  (let* ( (parts    (org-protocol-split-data data))
          (path     (car parts))
          (filename (cadr parts)) )
;;    (interactive)
    (message (concat "path=" path ", filename=" filename))
;;    (insert (org-make-link path filename))
    )
  nil)

(defun hello-world (data)
  "Say hello to the world."
  (message data)
nil)

(add-to-list 'org-protocol-protocol-alist
             '("Hello World"
               :protocol "hello-world"
               :function hello-world))

;; Sorting subtrees

(defun sbw/org-multisort (&rest criteria)
  "Sort subtree by multiple criteria. See org-sort-entries for sort types."
  (interactive)
  (mapc #'(lambda (x) (org-sort-entries nil x))
    (reverse criteria)))

(defun sbw/org-sort-subtree ()
  "Sort the current subtree by TODO state, priority, scheduled date, deadline, then alphabetic."
  (interactive)
  (if (org-clocking-p)
    (message "Currently clocked in on a task. Clock out and re-run the command to sort the subtree.")
    (save-excursion
      (sbw/org-multisort ?o ?p ?s ?d ?a)
      (hide-subtree)
      (org-cycle))))

;; Helper functions for extracting data from org buffers

(defun sbw/org-heading-points ()
  "Returns a list of the points of all the headings in the current org-mode buffer."
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

(defun sbw/org-strip-urls (s)
  "Return string s with any URLs replaced with their descriptions."
  (let* ( (str s) )
    (while (string-match org-bracket-link-regexp str)
      (let* ( (url   (match-string 1 str))
              (descr (match-string 3 str))
              (repl  (if descr descr url)) )
        (setq str
          (replace-regexp-in-string (regexp-quote (match-string 0 str)) repl str))))
    str))

(defun sbw/extract-string (x)
  "Returns a string extracted from the org property."
  (when x
    (-> x
      (substring-no-properties)
      (sbw/org-strip-urls))))

(defun sbw/extract-timestamp (x)
  "Returns a timestamp extracted from the org property."
  (when x
    (date-to-time x)))

(defun sbw/org-extract-heading-summary (x)
  "Returns a summary of the org-mode heading at point x."
  (let* ((summary (sbw/hash-table)))
    (save-excursion
      (goto-char x)
      (puthash :filename (buffer-file-name) summary)
      (puthash :point    x summary)
      (puthash :category (sbw/extract-string (org-entry-get-with-inheritance "CATEGORY")) summary)
      (puthash :state    (sbw/extract-string (org-get-todo-state)) summary)
      (puthash :tags     (sbw/extract-string (org-get-tags-at)) summary)
      (puthash :heading  (sbw/extract-string (org-get-heading nil t)) summary)
      (puthash :level    (funcall outline-level) summary)
      (puthash :closed   (sbw/extract-timestamp  (cdr (assoc "CLOSED" (org-entry-properties)))) summary))
    summary))

(defun sbw/org-heading-summaries (fnam)
  "Returns summaries for all the headings in file fnam. See sbw/org-extract-heading-summary for a description of the content of a summary."
  (set-buffer (find-file-noselect fnam))
  (-map 'sbw/org-extract-heading-summary (sbw/org-heading-points)))

;; Sorting all subtrees

(defun sbw/current-line ()
  "Returns the text from the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun sbw/sort-all-subtrees ()
  "Sorts all the subtrees in the current org-mode buffer."
  (interactive)
  (-each
    (->> (sbw/org-heading-summaries (buffer-file-name))
      (-filter (lambda (x) (equal 1 (gethash :level x)))))
    (lambda (x)
      (goto-char (gethash :point x))
      (message (format "Sorting subtree under [%s]" (sbw/current-line)))
      (ignore-errors (sbw/org-sort-subtree)))))

;; Reformat the current org-mode buffer

(defun sbw/org-mode-reformat ()
  "Reformat the current org-mode buffer, updating dynamic blocks, aligning tags, sorting subtrees."
  (interactive)
  (org-update-all-dblocks)
  (sbw/right-align-tags)
  (sbw/sort-all-subtrees)
  nil)

;; Weekly report

(defun sbw/generate-report-for-task-category (category completed-tasks)
  (concat
    (sbw/heading-two category)
    "\n"
    (apply 'concat
      (-map
        (lambda (x) (concat " - " (gethash :heading x)  "\n"))
        completed-tasks))
    "\n"))

(defun sbw/generate-report-for-completed-tasks (org-files filter-func)
  "Returns a summary of the completed tasks in the specified period."
  (let* ( (extract-heading-summaries (lambda (x) (-mapcat 'sbw/org-heading-summaries x)))
          (prune-irrelevant-tasks    (lambda (x) (-filter filter-func x)))
          (group-by-category         (lambda (x) (sbw/collect-by (lambda (y) (gethash :category y)) x)))
          (generate-category-report  (lambda (x) (sbw/map-hash 'sbw/generate-report-for-task-category x))) )
    (apply 'concat
      (->> org-files
        (funcall extract-heading-summaries)
        (funcall prune-irrelevant-tasks)
        (funcall group-by-category)
        (funcall generate-category-report)))))

(defun sbw/adjust-date-by (date n)
  "Returns a timestamp for the specified date plus n days."
  (days-to-time (+ (time-to-number-of-days date) n)))

(defun sbw/is-closed-between? (start end x)
  "Returns t if task x was closed between start and end."
  (let* ( (closed (gethash :closed x)) )
    (and
      closed
      (time-less-p start closed)
      (time-less-p closed end))))

(defun sbw/generate-report-for-period (org-files start end)
  "Returns a report for the specified period."
  (let* ( (format-date (-partial 'format-time-string "%A %e %B %Y")) )
    (concat
      (sbw/heading-one (concat "Review for " (funcall format-date start) " to " (funcall format-date end)))
      "\n"
      (sbw/generate-report-for-completed-tasks org-files (-partial 'sbw/is-closed-between? start end))
      "\n")))

(setq sbw/org-report-dir
  "C:/Users/IBM_ADMIN/my_local_stuff/home/my_stuff/srcs/org/reports")

(defun sbw/report-filename (start end title)
  "Returns the filename for a report in standard form."
  (let* ( (format-date (-partial 'format-time-string "%Y%m%d")) )
    (format "%s/%s-%s-to-%s.txt"
      sbw/org-report-dir
      title
      (funcall format-date start)
      (funcall format-date end))))

(defun sbw/generate-weekly-report ()
  "Returns the weekly report."
  (interactive)
  (let* ( (base   (apply 'encode-time (org-read-date-analyze "-sat" nil '(0 0 0))))
          (start  (sbw/adjust-date-by base -6))
          (end    (sbw/adjust-date-by base  1))
          (report (sbw/generate-report-for-period sbw/all-org-files start end))
          (fnam   (sbw/report-filename start end "weekly-report")) )
    (with-temp-file fnam (insert report))
    (message (format "Created '%s'" fnam))
    (find-file fnam)
    nil))


;; Stuff to rationalise


(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (;:foreground "PaleGreen"   
                 :weight normal
                 :strike-through t))))
 '(org-headline-done 
            ((((class color) (min-colors 16) (background dark)) 
               (; :foreground "LightSalmon"
                 :strike-through t))))
  )

;(custom-set-faces
; '(org-done          ((t (:background unspecified :foreground unspecified :weight normal :strike-through t :inherit (sbw-dark-tag)))))
; '(org-headling-done ((t (:background unspecified :foreground unspecified :weight normal :strike-through t :inherit (sbw-dark-normal)))))
; )





;; Notify appointment reminders using Growl

(defun sbw/growl-message (msg)
  "Display msg using Growl."
  (call-process "c:\\Program Files (x86)\\Growl for Windows\\growlnotify.exe" nil 0 nil "/s:true" "/t:emacs" msg))

(defun sbw/appt-disp-window (min-to-app new-time appt-msg)
  (sbw/growl-message (concat "Reminder: " appt-msg))
  (appt-disp-window min-to-app new-time appt-msg))

(setq appt-disp-window-function 'sbw/appt-disp-window)


;; Archiving

;; http://stackoverflow.com/questions/7509463/how-to-move-a-subtree-to-another-subtree-in-org-mode-emacs
;; C-c C-x C-s
;; C-c C-x C-a

;; Create target file
;; Create top-level target heading in target file
;; Refile to target
;; Promote subtree
;; Remove target heading

;(setq org-refile-allow-creating-parent-nodes (quote confirm))
(defun sbw/archive-file-name ()
  (concat
    org-directory
    "archive/"
    "archive-"
    (file-name-nondirectory (buffer-file-name))))

;; org-refile-use-outline-path
;;  include filename




  

  
;(setq org-refile-allow-creating-parent-nodes t)
;(setq org-refile-use-outline-path 'full-file-path)

;(defun sbw/test ()
;  (interactive) 
;  ;(org-refile nil nil '("one.org" . ("two.org" . "three.org")))
;  (print (org-refile-get-location))
;  )

;(setq org-refile-allow-creating-parent-nodes nil)
;(setq org-refile-use-outline-path nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sbw/summary-remaining-time (end-time)
  (format-time-string "%H:%M:%S" (time-subtract end-time (current-time))))

(defun sbw/summary-update-timer ()
  (setq sbw/summary-mode-line-string (concat " <" (sbw/summary-remaining-time sbw/summary-end-time) ">"))
  (force-mode-line-update))

(defun sbw/summary-set-mode-line (value)
  (cond
    ((equal value :off)
      (setq sbw/summary-mode-line-timer nil)
      (delq 'sbw/summary-mode-line-string global-mode-string))

    ((equal value :on)
      (setq sbw/summary-mode-line-timer nil)
      (when (not (memq 'sbw/summary-mode-line-string global-mode-string))
        (setq global-mode-string (append global-mode-string '(sbw/summary-mode-line-string)))))))

(defun sbw/summary-set-timer (value)
  (cond
    ((equal value :off)
      (when sbw/summary-timer
        (cancel-timer sbw/summary-timer)
        (setq sbw/summary-timer nil)))

    ((equal value :on)
      (when sbw/summary-timer
        (cancel-timer sbw/summary-timer))
      (setq sbw/summary-timer (run-with-timer 1 1 'sbw/summary-update-timer)))))

(defun sbw/summary (value)
  (cond
    ((equal value :off)
      (sbw/summary-set-mode-line :off)
      (sbw/summary-set-timer :off)
      (setq sbw/summary-end-time nil)
      (message "Summary timer stopped."))

    ((equal value :on)
      (setq sbw/summary-timer nil)
      (setq sbw/summary-end-time (time-add (seconds-to-time 30) (current-time)))
      (sbw/summary-update-timer)
      (sbw/summary-set-mode-line :on)
      (sbw/summary-set-timer :on)
      (message "Summary timer started."))))

(provide 'sbw-setup-org-mode)
