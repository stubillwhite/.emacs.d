(require 'use-package)

(use-package org
  :init
  (progn
    (require 'org-clock)
    
    ;; Default to clean view with no leading asterisks for indentation
    (setq-default org-startup-indented t)

    ;; Org files

    (when (eq system-type 'gnu/linux)
      (setq org-directory "~/Dropbox/Private/org"))

    (when (eq system-type 'windows-nt)
      (setq org-directory "C:/Users/IBM_ADMIN/Dropbox/Private/org"))
    
    (defun sbw/org-files (&rest dirs)
      "Return a list of the org files in directories DIRS."
      (-mapcat
        (lambda (x) (directory-files x :absolute org-agenda-file-regexp))
        (apply 'list dirs)))

    (defun sbw/org-find-org-files ()
      "Scan org-directory for org files."
      (interactive)
      (setq sbw/org-personal-files    (sbw/org-files (concat org-directory "/current/personal")))
      (setq sbw/org-non-project-files (sbw/org-files (concat org-directory "/current/non-project")))
      (setq sbw/org-work-files        (sbw/org-files (concat org-directory "/current/work")))
      (setq sbw/org-all-files         (append sbw/org-personal-files sbw/org-work-files sbw/org-non-project-files (list)))
      (setq sbw/org-refile-targets    (-filter (lambda (x) (not (-contains? sbw/org-non-project-files x))) sbw/org-all-files))
      (setq org-agenda-files          sbw/org-all-files)
      (setq org-default-notes-file    (concat org-directory "/current/non-project/incoming.org"))
      (setq org-refile-targets        (quote ((sbw/org-refile-targets :maxlevel . 1)))))

    (sbw/org-find-org-files)
    
    ;; General settings

    (setq 
      org-clock-into-drawer         t        ;; Clock into drawers
      org-ellipsis                  "\u2026" ;; Small ellipsis character
      org-agenda-fontify-priorities nil      ;; Don't let priority change task representation
      org-indent-mode               t        ;; Use indent mode
      org-log-into-drawer           t        ;; Log into drawers
      org-log-done                  'time    ;; Timestamp task completion so it can be used in reports
      org-M-RET-may-split-line      nil      ;; Don't split lines
      org-return-follows-link       t        ;; Easy link navigation
      org-use-property-inheritance  t        ;; Child items should inherit all from parents
      org-default-priority          ?B       ;; Default priority for unprioritised items
      appt-display-interval         5        ;; Reminder for an appointment every five minutes...
      appt-message-warning-time     15       ;; ...starting fifteeen minutes before it is due
      )

    ;; Don't use any tags yet. This needs some attention
    (setq org-tag-alist nil)

    (setq org-todo-keywords
      '("TODO(t)" "STARTED(s)" "BLOCKED(b)" "POSTPONED(p)" "|" "DONE(d!)" "CANCELLED(c)"))

    (setq org-drawers
      '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES"))

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

    (setq org-clock-in-switch-to-state (quote sbw/clock-in-switch-to-started)))

  :config
  (progn
    ;; Link type for opening a file in a running Eclipse instance

    (org-add-link-type "eclipse"
      (lambda (path)
        (start-process-shell-command "Eclipse" nil (concat "c:\\Program Files\\DevComponents\\Eclipse\\eclipse.exe --launcher.openFile " path))))

    ;; Link type for opening a file in Vim

    (org-add-link-type "vim"
      (lambda (path)
        (start-process-shell-command "Vim" nil (concat "gvim.exe " path))))

    ;; Link type for opening a file in Atom

    (org-add-link-type "atom"
      (lambda (path)
        (start-process-shell-command "Atom" nil (concat "C:\\Users\\IBM_ADMIN\\my_local_stuff\\home\\utils\\bin\\atom-windows\\Atom\\Atom.exe " path))))
         
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
      '( (agenda . " %i %-20:c%?-20t% s")
         (timeline . "  % s")
         (todo . " %i %-20:c")
         (tags . " %i %-20:c")
         (search . " %i %-20:c")) )

    (setq org-agenda-remove-tags 1)

    (setq org-agenda-custom-commands
      '( ("c" . "Custom agenda")
         ("cw" "Work agenda"
           ( (agenda ""
               ( (org-agenda-ndays 7)
                 (org-agenda-files sbw/org-work-files)
                 ))
                  
             (tags-todo "+PRIORITY=\"A\""
               ( (org-agenda-overriding-header (sbw/make-title-string "High priority tasks"))
                 (org-agenda-files sbw/org-work-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
                 (org-agenda-files sbw/org-work-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "+PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
                 (org-agenda-files sbw/org-work-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             )
           nil
           ("C:/Users/IBM_ADMIN/Dropbox/Private/org-mode/work-agenda.html"))
     
         ("cp" "Personal agenda"
           ( (agenda ""
               ( (org-agenda-ndays 7)
                 (org-agenda-files sbw/org-personal-files)
                 ))
         
             (tags-todo "+PRIORITY=\"A\""
               ( (org-agenda-overriding-header (sbw/make-title-string "High priority tasks"))
                 (org-agenda-files sbw/org-personal-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
                 (org-agenda-files sbw/org-personal-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "+PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
                 (org-agenda-files sbw/org-personal-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))
             )
           nil
           ("C:/Users/IBM_ADMIN/Dropbox/Private/org-mode/personal-agenda.html"))
         ))

    ;; Appointments
    ;; Refresh at startup and when the agenda is displayed

    (defun sbw/org-refresh-appointments-from-agenda ()
      "Update the appointment list from the agenda."
      (interactive)
      (setq appt-time-msg-list nil)
      (org-agenda-to-appt))

    (add-hook 'org-finalize-agenda-hook 'sbw/org-refresh-appointments-from-agenda 'append)
    ;(sbw/org-refresh-appointments-from-agenda)

    (appt-activate t)

    ;; org-protocol

    (require 'org-protocol)

    (setq org-protocol-default-template-key nil)

    (setq org-capture-templates
      '( ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?%a\n%i")
         ("l" "Link" entry (file+olp org-default-notes-file "Links")
           "* TODO %?%a\n%i")
         ("j" "RTC task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?%a")
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
      "Return a list of the points of all the headings in the current org-mode buffer."
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
      "Return a string extracted from the org property."
      (when x
        (-> x
          (substring-no-properties)
          (sbw/org-strip-urls))))

    (defun sbw/extract-timestamp (x)
      "Return a timestamp extracted from the org property."
      (when x
        (date-to-time x)))

    (defun sbw/org-extract-heading-summary (x)
      "Return a summary of the org-mode heading at point x."
      (let* ((summary (sbw/ht-create)))
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

    (defun sbw/white-test ()
      (interactive)
      (print (sbw/org-extract-heading-summary (point))))

    (defun sbw/org-heading-summaries (fnam)
      "Return summaries for all the headings in file fnam. See sbw/org-extract-heading-summary for a description of the content of a summary."
      (set-buffer (find-file-noselect fnam))
      (-map 'sbw/org-extract-heading-summary (sbw/org-heading-points)))

    ;; Sorting all subtrees

    (defun sbw/current-line ()
      "Return the text from the current line."
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
      (let* ( (to-string       (lambda (x) (sbw/ht-get x :heading)))
              (task-comparator (lambda (x y) (string< (funcall to-string x) (funcall to-string y)))) )
        (concat
          (sbw/heading-two category)
          "\n"
          (apply 'concat
            (-map
              (lambda (x) (concat " - " (funcall to-string x)  "\n"))
              (-sort task-comparator completed-tasks)))
          "\n")))

    (defun sbw/generate-report-for-completed-tasks (org-files filter-func)
      "Return a summary of the completed tasks in the specified period."
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
      "Return a timestamp for the specified date plus n days."
      (days-to-time (+ (time-to-number-of-days date) n)))

    (defun sbw/is-completed-between? (start end x)
      "Return t if task x was completed between start and end."
      (let* ( (closed (gethash :closed x)) )
        (and
          closed
          (time-less-p start closed)
          (time-less-p closed end)
          (equal (gethash :state x) "DONE"))))

    (defun sbw/generate-report-for-period (org-files start end)
      "Return a report for the specified period."
      (let* ( (format-date (-partial 'format-time-string "%A %e %B %Y")) )
        (concat
          (sbw/heading-one (concat "Review for " (funcall format-date start) " to " (funcall format-date end)))
          "\n"
          (sbw/generate-report-for-completed-tasks org-files (-partial 'sbw/is-completed-between? start end))
          "\n")))

    (setq sbw/org-report-dir
      (concat org-directory "/reports"))

    (defun sbw/report-filename (start end title)
      "Return the filename for a report in standard form."
      (let* ( (format-date (-partial 'format-time-string "%Y%m%d")) )
        (format "%s/%s-%s-to-%s.txt"
          sbw/org-report-dir
          title
          (funcall format-date start)
          (funcall format-date end))))

    (defun sbw/generate-weekly-report (base)
      "Generate the weekly report, write it out, and open it for review."
      (let* ( (start  (sbw/adjust-date-by base -6))
              (end    (sbw/adjust-date-by base  1))
              (report (sbw/generate-report-for-period sbw/org-all-files start end))
              (fnam   (sbw/report-filename start end "weekly-report")) )
        (with-temp-file fnam (insert report))
        (message (format "Created '%s'" fnam))
        (find-file fnam)
        nil))

    (defun sbw/generate-weekly-report-for-previous-week ()
      "Generate the weekly report for the previous week."
      (interactive)
      (sbw/generate-weekly-report (apply 'encode-time (org-read-date-analyze "-sat" nil '(0 0 0)))))

    (defun sbw/generate-weekly-report-for-current-week ()
      "Generate the weekly report for the current week."
      (interactive)
      (sbw/generate-weekly-report (apply 'encode-time (org-read-date-analyze "+sat" nil '(0 0 0)))))

    (defun sbw/generate-monthly-report (base)
      "Generate the monthly report, write it out, and open it for review."
      (let* ( (decomp-base (sbw/decompose-time base))
              (last-day    (calendar-last-day-of-month (sbw/ht-get decomp-base :month) (sbw/ht-get decomp-base :year)))
              (start       (sbw/compose-time (sbw/ht-merge decomp-base (sbw/ht-create :day 1))))
              (end         (sbw/compose-time (sbw/ht-merge decomp-base (sbw/ht-create :day last-day))))
              (report      (sbw/generate-report-for-period sbw/org-all-files start end))
              (fnam        (sbw/report-filename start end "monthly-report")))
        (with-temp-file fnam (insert report))
        (message (format "Created '%s'" fnam))
        (find-file fnam)
        nil))

    (defun sbw/generate-monthly-report-for-previous-month ()
      "Generate the monthly report for the previous month."
      (interactive)
      (let* ( (decomp-curr-time (sbw/decompose-time (current-time)))
              (prev-month       (sbw/dec (sbw/ht-get decomp-curr-time  :month)))
              (base             (sbw/compose-time (sbw/ht-merge decomp-curr-time (sbw/ht-create :month prev-month)))) )
        (sbw/generate-monthly-report base)))

    (defun sbw/generate-monthly-report-for-current-month ()
      "Generate the monthly report for the current month."
      (interactive)
      (let* ( (base (current-time)) )
        (sbw/generate-monthly-report base)))

    ;; Stuff to rationalise

    (setq org-fontify-done-headline t)
    (custom-set-faces
      '(org-done ((t (                  ;:foreground "PaleGreen"   
                       :weight normal
                       :strike-through t))))
      '(org-headline-done 
         ((((class color) (min-colors 16) (background dark)) 
            (                           ; :foreground "LightSalmon"
              :strike-through t))))
      )

                                        ;(custom-set-faces
                                        ; '(org-done          ((t (:background unspecified :foreground unspecified :weight normal :strike-through t :inherit (sbw-dark-tag)))))
                                        ; '(org-headling-done ((t (:background unspecified :foreground unspecified :weight normal :strike-through t :inherit (sbw-dark-normal)))))
                                        ; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Notify appointment reminders using Growl

    (defun sbw/growl-message (msg)
      "Display msg using Growl."
      (call-process "c:\\Program Files (x86)\\Growl for Windows\\growlnotify.exe" nil 0 nil "/s:true" "/t:emacs" msg))

                                        ;(defun sbw/appt-disp-window (min-to-app new-time appt-msg)
                                        ;  (sbw/growl-message (concat "Reminder: " appt-msg))
                                        ;  (appt-disp-window min-to-app new-time appt-msg))

                                        ;(setq appt-disp-window-function 'sbw/appt-disp-window)

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
        "/archive/"
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

    (setq org-clock-heading-function
      (lambda () (sbw/truncate-string (nth 4 (org-heading-components)) 30)))))

(provide 'sbw-configure-org-mode)
