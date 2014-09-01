(require 'org-clock)

(defun sbw/windows-process-status (name)
  (let* ( (cmd    (concat "sc query \"" name "\""))
          (str    (shell-command-to-string cmd))
          (regex  "SERVICE_NAME: \\(.+\\)\n.*\n.*STATE\\W+: \\(\\w+\\)\\W+\\(\\w+\\)")
          (status (sbw/hash-table)) )
    (when (string-match regex str)
      (puthash :name   (match-string 1 str) status)
      (puthash :code   (match-string 2 str) status)
      (puthash :status (match-string 3 str) status)
      status)))

(defun sbw/windows-process-start (name)
  (let* ( (cmd (concat "sc start \"" name "\"")) )
    (shell-command-to-string cmd)))

(defun sbw/ensure-symantec-is-running ()
  (let* ( (proc-name  "SepMasterService")
          (proc-status (sbw/windows-process-status proc-name)) )
    (when (string-equal "STOPPED" (gethash :status proc-status))
      (sbw/windows-process-start proc-name)
      (message "Symantec service is not currently running. Starting it."))))

(sbw/ensure-symantec-is-running)

;; Default to clean view with no leading asterisks for indentation
(setq-default org-startup-indented t)

;; Org files

(setq org-directory 
      "c:/users/ibm_admin/my_local_stuff/home/my_stuff/srcs/org/")

(defun sbw/org-file (&rest args)
  (mapcar (-partial 'concat org-directory) args))

(defconst sbw/personal-files
  (sbw/org-file "todo-personal.org" "personal/architecture.org" "personal/car.org" "personal/edinburgh.org" "personal/health.org" "personal/music.org" "personal/go.org"))

(defconst sbw/calendar-files
  (sbw/org-file "google-calendar.org"))

(defconst sbw/work-files
  (sbw/org-file "todo-work.org" "timesheet.org" "work/apollo.org" "work/aurora.org"))

(defconst sbw/planning-files
  (sbw/org-file "incoming.org" "weekly-plan.org"))

(defconst sbw/habits-files
  (sbw/org-file "habits.org"))

(defconst sbw/org-files
  (append sbw/personal-files sbw/work-files sbw/planning-files sbw/calendar-files sbw/habits-files (list)))

(setq org-agenda-files sbw/org-files)

(defconst sbw/org-refile-targets
  (-filter (lambda (x) (not (-contains? sbw/calendar-files x))) sbw/org-files))

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
;;  org-replace-disputed-keys     t           ;; Prevent org-mode from binding shift-cursor keys
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

;;
;; Pull in org
;; Doing this earlier seems to result in some of the above settings being lost
;; TODO Investigate what's going on there
;;

(require 'org)

;; Link type for opening a file in a running Eclipse instance

(org-add-link-type "eclipse"
  (lambda (path)
    (start-process "Eclipse" nil "c:\\Program Files\\DevComponents\\Eclipse\\eclipse.exe" "--launcher.openFile" path)))

;; Temporary -- Link type for opening in Aurora vintage Eclipse
(org-add-link-type "aurora"
  (lambda (path)
    (start-process "Aurora" nil "c:\\dev_fp\\eclipse-for-sdk\\eclipse.exe" "--launcher.openFile" path)))

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
             (org-agenda-prefix-format "%-10c %-10T")
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
             (org-agenda-files sbw/work-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-prefix-format "%-10c %-10T")                          
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "+PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
             (org-agenda-files sbw/work-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-prefix-format "%-10c %-10T")                          
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
             (org-agenda-prefix-format "%-10c %-10T")             
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
             (org-agenda-files sbw/personal-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-prefix-format "%-10c %-10T")
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))

         (tags-todo "+PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
             (org-agenda-files sbw/personal-files)
             (org-agenda-todo-ignore-scheduled t)
             (org-agenda-prefix-format "%-10c %-10T")
             (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
             ))
         ))

     ("2" "Weekly review sans routines" agenda ""
       ( (org-agenda-log-mode t)
         (org-agenda-span 7)         
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

;; Helper to apply a function to all headings in a buffer

(defun sbw/goto-first-heading ()
  (interactive)
  (beginning-of-buffer)
  (when (not (outline-on-heading-p))
    (outline-next-heading))
  nil)

(defun sbw/current-line ()
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun sbw/map-to-headings (f)
  "Apply f(p) to all headings in the buffer, where p is the point in the buffer." 
  (save-excursion
    (sbw/goto-first-heading)
    (funcall f (point))
    (while (outline-next-heading)
      (funcall f (point))))
  nil)

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

(setq edebug-trace t)

(defun sbw/generate-report-for-task-category (category completed-tasks)
  (concat
    (sbw/heading-two category)
    "=\n"
    (apply 'concat
      (-map
        (lambda (x) (concat " - (" (gethash :heading x) ")\n"))
        completed-tasks))
    "=\n"))

(defun sbw/generate-report-for-completed-tasks (filter-func)
  "Returns a summary of the completed tasks in the specified period."
  (let* ( (extract-heading-summaries (lambda (x) (-mapcat 'sbw/org-heading-summaries x)))
          (prune-irrelevant-tasks    (lambda (x) (-filter filter-func x)))
          (group-by-category         (lambda (x) (sbw/collect-by (lambda (y) (gethash :category y)) x)))
          (generate-category-report  (lambda (x) (sbw/map-hash 'sbw/generate-report-for-task-category x))) )
    (apply 'concat
      (->> sbw/org-files
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

(defun sbw/generate-report-for-period (start end)
  "Returns a report for the specified period."
  (let* ( (format-date (-partial 'format-time-string "%A %e %B %Y")) )
    (concat
      (sbw/heading-one (concat "Review for " (funcall format-date start) " to " (funcall format-date end)))
      "\n"
      (sbw/generate-report-for-completed-tasks (-partial 'sbw/is-closed-between? start end))
      "\n")))

(setq sbw/org-report-dir
  "C:/Users/IBM_ADMIN/my_local_stuff/home/my_stuff/srcs/org/reports")

(defun sbw/generate-weekly-report ()
  "Returns the weekly report."
  (let* ( (base        (apply 'encode-time (org-read-date-analyze "-sat" nil '(0 0 0))))
          (start       (sbw/adjust-date-by base -6))
          (end         (sbw/adjust-date-by base  1))
          (report      (sbw/generate-report-for-period start end))
          (format-date (-partial 'format-time-string "%Y%m%d"))
          (fnam        (format "%s/weekly-report-%s-to-%s.txt"
                         sbw/org-report-dir
                         (funcall format-date start)
                         (funcall format-date end))))
    (with-temp-file fnam (insert report))
    (message (format "Created '%s'" fnam))
    (find-file fnam)
    nil))






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





(add-to-list 'org-modules 'org-habit)

;;(sbw/growl-message "Hello this is a message from emacs")



(defun sbw/white-test ()
  "Just for testing"
  (interactive)
  (sbw/generate-weekly-report))

;; Notify appointment reminders using Growl

(defun sbw/growl-message (msg)
  "Display msg using Growl."
  (call-process "c:\\Program Files (x86)\\Growl for Windows\\growlnotify.exe" nil 0 nil "/s:true" "/t:emacs" msg))

(defun sbw/appt-disp-window (min-to-app new-time appt-msg)
  (sbw/growl-message (concat "Reminder: " appt-msg))
  (appt-disp-window min-to-app new-time appt-msg))

(setq appt-disp-window-function 'sbw/appt-disp-window)

(provide 'sbw-setup-org-mode)
