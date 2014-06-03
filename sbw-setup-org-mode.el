;; Default to clean view with no leading asterisks for indentation
(setq-default org-startup-indented t)

;; Org directory
(setq org-directory 
      "c:/users/ibm_admin/my_local_stuff/home/my_stuff/srcs/org/")

;; Agenda files
(defun sbw-org/org-files (&rest args)
  (mapcar (lambda (x) (concat org-directory x)) args))

(defconst sbw-org/personal-files
  (sbw-org/org-files "todo-personal.org"))

(defconst sbw-org/work-files
  (sbw-org/org-files "todo-work.org" "timesheet.org"))

(defconst sbw-org/planning-files
  (sbw-org/org-files "incoming.org" "weekly-plan.org"))

(setq org-agenda-files
  (append sbw-org/personal-files sbw-org/work-files sbw-org/planning-files (list)))

(setq org-default-notes-file (concat org-directory "incoming.org") )

;; General settings

(setq 
  org-clock-into-drawer         t           ;; Clock into drawers
  org-ellipsis                  "\u2026"    ;; Small ellipsis character
  org-indent-mode               t           ;; Use indent mode
  org-log-into-drawer           t           ;; Log into drawers
  org-M-RET-may-split-line      nil         ;; Don't split lines
  org-replace-disputed-keys     t           ;; Prevent org-mode from binding shift-cursor keys
  org-return-follows-link       t           ;; Easy link navigation
  org-use-property-inheritance  t           ;; Child items should inherit all from parents
  org-default-priority          ?B          ;; Default priority for unprioritised items
)

;; Clocking
;; Clock into a task should switch state to started if it is still in a stalled state

(defun sbw-org-mode/clock-in-switch-to-started (state) 
  "When clocking in, switch the task state STARTED if it is still in a stalled state." 
  (if (and (or (string-equal state "TODO")
               (string-equal state "BLOCKED")
               (string-equal state "POSTPONED"))
           (not (string-equal (buffer-name) "*Remember*"))) 
    "STARTED" 
    nil))

(setq org-clock-in-switch-to-state (quote sbw-org-mode/clock-in-switch-to-started))

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

;; Link type for opening a file in a running Vim instance

(org-add-link-type "vim"
  (lambda (path)
    (start-process "Vim" nil "gvim.exe" path)))
     
;; Tag alignment
;; Ensure new tags are created right-aligned based on the window size, and
;; provide a handy function to re-align all tags in the buffer

(defun sbw-org-mode/set-org-tags-column-based-on-window-size ()
  "Set org-tags-column to right-align based on window size. Assumes that org-ellipsis is a string."
  (setq org-tags-column (- (- (window-width) (length org-ellipsis)))))

;; (add-hook 'window-configuration-change-hook 'sbw-org-mode/set-org-tags-column-based-on-window-size)

(defun sbw-org-mode/right-align-tags ()
  "Right-align the all tags in the buffer."
  (interactive)
  (sbw-org-mode/set-org-tags-column-based-on-window-size)
  (org-align-all-tags)
  (redisplay t))



(defun org-sort-list-by-checkbox-type-1 ()
  (if (looking-at org-list-full-item-re)
    (cdr (assoc (match-string 3)
           '(("[X]" . 1) ("[-]" . 2) ("[ ]" . 3) (nil . 4))))
    4))

(defun sbw-org/make-title-string (title)
  (concat "\n" title "\n" (make-string (length title) ?-) "\n"))

(defun sbw-org/org-agenda-common-display ()
  (org-agenda-overriding-header "\nFoo\n---"))

;; TODO Look into http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/

(setq org-agenda-custom-commands
  '(
     
     ("P" "Personal agenda"
       ( (agenda "" ((org-agenda-ndays 7)))
;;         (todo (sbw-org/org-agenda-common-display))
         (tags-todo "+PRIORITY=\"A\""
           ((org-agenda-overriding-header (sbw-org/make-title-string  "High-priority tasks"))))

;;         (org-agenda-files sbw-org/personal-files)
         (tags-todo "+PRIORITY=\"A\""
           ((org-agenda-overriding-header (sbw-org/make-title-string "foo"))))

         ))
     
     ("W" "Work agenda"
       ( (agenda "" ((org-agenda-ndays 7)))
         (org-agenda-files sbw-org/work-files)
         
         (tags "+PRIORITY=\"A\""
           ( (org-agenda-overriding-header (sbw-org/make-title-string "High priority tasks"))
             (org-agenda-files sbw-org/work-files)
             ))

         (tags "+PRIORITY=\"B\""
           ( (org-agenda-overriding-header (sbw-org/make-title-string  "Normal priority tasks"))
             (org-agenda-files sbw-org/work-files)            
             ))

         (tags "+PRIORITY=\"C\""
           ( (org-agenda-overriding-header (sbw-org/make-title-string  "Low priority tasks"))
             (org-agenda-files sbw-org/work-files)            
             ))

         ))


     
     ("p" . "Priorities")
     ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
     ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
     ("pc" "C items" tags-todo "+PRIORITY=\"C\"")

     ("J" "My agenda"
       ( (agenda "" ((org-agenda-ndays 7)))
         (tags-todo "+PRIORITY=\"A\""
           ((org-agenda-overriding-header "\nHigh-priority tasks\n-------------------\n")))
         (todo "BLOCKED")

       (todo "TODO"
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n")))

         
  
         ))
     
     ))


(setq org-agenda-fontify-priorities t)

;; Key bindings
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; Notes configuration

;; (setq org-protocol-default-template-key "l")
;; (setq org-capture-templates
;;  '(("t" "Todo" entry (file+headline "/path/to/notes.org" "Tasks")
;;         "* TODO %?\n  %i\n  %a")
;;    ("l" "Link" entry (file+olp "/path/to/notes.org" "Web Links")
;;         "* %a\n %?\n %i")
;;    ("j" "Journal" entry (file+datetree "/path/to/journal.org")
;;         "* %?\nEntered on %U\n  %i\n  %a")))
;; 
;; ;; Archive as much information as possible
;; (setq org-archive-save-context-info
;;   '(time file ltags itags todo category olpath))

;;;; ;; Not working :(
;;;; ;;
;;;; ;; (add-hook 'org-mode-hook
;;;; ;;       '(lambda ()
;;;; ;;          (delete '("\\.pdf\\'" . default) org-file-apps)
;;;; ;;          (add-to-list 'org-file-apps '("\\.pdf\\'" . "explorer %s"))))

(provide 'sbw-setup-org-mode)
