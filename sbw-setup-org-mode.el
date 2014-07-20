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
  (sbw/org-files "todo-personal.org"))

(defconst sbw/calendar-files
  (sbw/org-files "google-calendar.org"))

(defconst sbw/work-files
  (sbw/org-files "todo-work.org" "timesheet.org"))

(defconst sbw/planning-files
  (sbw/org-files "incoming.org" "weekly-plan.org"))

(setq org-agenda-files
  (append sbw/personal-files sbw/work-files sbw/planning-files sbw/calendar-files (list)))

(setq org-default-notes-file (sbw/org-file "incoming.org"))

;; General settings

(setq 
  org-clock-into-drawer         t           ;; Clock into drawers
  org-ellipsis                  "\u2026"    ;; Small ellipsis character
  org-agenda-fontify-priorities nil         ;; Don't let priority change task representation
  org-indent-mode               t           ;; Use indent mode
  org-log-into-drawer           t           ;; Log into drawers
  org-M-RET-may-split-line      nil         ;; Don't split lines
;;  org-replace-disputed-keys     t           ;; Prevent org-mode from binding shift-cursor keys
  org-return-follows-link       t           ;; Easy link navigation
  org-use-property-inheritance  t           ;; Child items should inherit all from parents
  org-default-priority          ?B          ;; Default priority for unprioritised items
)

(setq org-todo-keywords
  '("TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)" "POSTPONED(p)"))

(setq org-drawers
  '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "NOTES"))

(setq org-archive-save-context-info
  '(time file ltags itags todo category olpath))

(setq org-refile-targets
  (quote ((org-agenda-files :maxlevel . 1))))

;(setq org-refile-targets
;  (let ((targets (sbw/filter (lambda (x) (not (member x sbw/calendar-files))) org-agenda-files)))
;    (quote ((targets . (:maxlevel . 1))))))

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
        "* TODO %?\n %i %a")
    ("l" "Link" entry (file+olp (sbw/org-file "incoming.org") "Links")
        "* TODO %a\n %?\n %i")
    ("j" "Jira task" entry (file+headline (sbw/org-file "timesheet.org") "Project-X")
        "* TODO [#A] %?%a")
   ))


(defun sbw/eclipse-link (data)
  "Push a link to open a file in Eclipse."
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
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun sbw/map-to-headings (f)
  "Apply f to all headings in the buffer."
  (save-excursion
    (sbw/goto-first-heading)
    (funcall f (sbw/current-line))
    (while (outline-next-heading)
      (funcall f (sbw/current-line))))
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
  (save-excursion
    (sbw/org-multisort ?o ?p ?s ?d ?a)
    (hide-subtree)
    (org-cycle)))

;; Sorting all subtrees

(defun sbw/is-top-level-heading-p ()
  (= (outline-level) 2))

(defun sbw/sort-all-subtrees ()
  (interactive)
  (sbw/map-to-headings
    '(lambda (x)
       (when (sbw/is-top-level-heading-p)
         (message (format "Sorting subtree under [%s]" (sbw/current-line)))
         (ignore-errors (sbw/org-sort-subtree))))))

;; Reformat the current org-mode buffer

(defun sbw/org-mode-reformat ()
  "Reformat the current org-mode buffer, updating dynamic blocks, aligning tags, sorting subtrees."
  (interactive)
  (org-update-all-dblocks)
  (sbw/right-align-tags)
  (sbw/sort-all-subtrees)
  nil)

(provide 'sbw-setup-org-mode)
