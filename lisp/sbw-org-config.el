;;; sbw/org-config.el --- Functions for managing my org-mode configuration

(require 's)
(require 'f)
(require 'dash)

;; Functions for finding files within my org-mode file structure, which is of the form:
;;
;;     org-directory
;;       project-one
;;         category-one
;;           foo.org
;;           bar.org
;;         category-two
;;           baz.org
;;       project-two
;;         category-three
;;           gonk.org
;;
;; This could all probably be done with one monolithic file and tags, but I like to keep stuff separate.

(defun sbw/org-config--categorise-files (files)
  (-reduce-from
    (lambda (acc x)
      (let* ( (path    (s-split "/" (f-relative x org-directory)))
              (stack   (car path))
              (project (cadr path)) )
        (sbw/ht-update-in acc (vector stack project) (lambda (y) (cons x y)))))
    (sbw/ht-create)
    files))

(defun sbw/org-config--find-and-categorise-files ()
  (let* ( (org-file?   (lambda (x) (s-ends-with? ".org" (f-filename x))))
          (all-files   (f-entries org-directory org-file? :recursive))
          (categorised (sbw/org-config--categorise-files all-files)) )
    (sbw/ht-create
      :all-files           all-files
      :categorised         categorised
      :selected-projects   nil
      :selected-categories nil
      :selected-files      all-files)))

(defun sbw/org-config--set-org-variables ()
  (setq
   org-agenda-files       (sbw/ht-get sbw/org-config :selected-files)
   org-default-notes-file (concat org-directory "/incoming.org")
   org-refile-targets     (quote ((org-agenda-files :maxlevel . 1))))
  sbw/org-config)

(defun sbw/org-config-refresh ()
  "Refresh the org-mode configuration."
  (interactive)
  (setq sbw/org-config (sbw/org-config--find-and-categorise-files))
  (sbw/org-config--set-org-variables))

(defvar sbw/org-config (sbw/org-config-refresh)
   "The org-mode configuration.")

(defun sbw/org-config-projects ()
  "Returns a list of org-mode projects."
  (sbw/ht-keys (sbw/ht-get sbw/org-config :categorised)))

(defun sbw/org-config-categories ()
  "Returns a list of org-mode categories."
  (seq-uniq
    (seq-mapcat
      'sbw/ht-keys
      (sbw/ht-vals (sbw/ht-get sbw/org-config :categorised)))))

(defun sbw/org-config-files (projects categories)
  "Returns the org-mode files filtered by the specified PROJECTS
and CATEGORIES, where PROJECTS and CATEGORIES are lists of string
names, or nil to indicate that all should be included."
  (let* ( (projects    (or projects   (sbw/org-config-projects)))
          (categories  (or categories (sbw/org-config-categories)))
          (filter-vals (lambda (ht ks) (sbw/ht-vals (sbw/ht-select-keys ht ks)))) )
    (apply (-partial 'seq-concatenate 'list)
      (seq-mapcat
        (lambda (x) (funcall filter-vals x categories))
        (funcall filter-vals (sbw/ht-get sbw/org-config :categorised) projects)))))

(defun sbw/org-config--select-projects-and-categories ()
  (let* ( (projects-source   (helm-build-sync-source "Projects"
                               :candidates (sbw/org-config-projects)
                               :action     (lambda (candidate) (helm-marked-candidates))))
          (categories-source (helm-build-sync-source "Categories"
                               :candidates (sbw/org-config-categories)
                               :action     (lambda (candidate) (helm-marked-candidates)))) )
    (list
     (helm :buffer "*helm org-config*" :sources projects-source)
     (helm :buffer "*helm org-config*" :sources categories-source))))

(defun sbw/org-config-select (projects categories)
  "Select the org-mode files filtered by the specified PROJECTS
and CATEGORIES, where PROJECTS and CATEGORIES are lists of string
names, or nil to indicate that all should be included. If called
interactively, prompt to select PROJECTS and CATEGORIES."
  (interactive (sbw/org-config--select-projects-and-categories))
  (setq sbw/org-config (-> sbw/org-config
                           (sbw/ht-assoc :selected-projects projects)
                           (sbw/ht-assoc :selected-categories categories)
                           (sbw/ht-assoc :selected-files (sbw/org-config-files projects categories))))
  (sbw/org-config--set-org-variables))

(defun sbw/org-config-new-file (project category)
  "Create a new file with specified PROJECT and CATEGORY, prompting for those values if rn interactively"
  (interactive "sProject: \nsCategory: ")
  (interactive (-map 'car (sbw/org-config--select-projects-and-categories)))
  (let* ( (content  (f-read-text (s-lex-format "${sbw/lisp-path}/sbw-org-review-new-file-template.org")))
          (path     (s-lex-format "${org-directory}/${project}/${category}.org")) )
    (f-mkdir (f-dirname path))
    (f-write (s-replace-all `(("${category}" . ,category)) content) 'utf-8 path)
    (sbw/org-config-refresh)
    (message "Created and added %s" path)))

;; Agenda

(defun sbw/org-config--title (title)
  (concat "\n" title "\n" (make-string (length title) ?-) "\n"))

(defmacro sbw/org-config-prioritised-tasks (binding title files)
  `(quote (,binding
           ,title
           ((todo "TODO|STARTED"
                  ((org-agenda-overriding-header (sbw/org-config--title "Open tasks"))
                   (org-agenda-files ,files)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                   (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if 'scheduled 'deadline)))))
            (todo "BLOCKED|POSTPONED"
                  ((org-agenda-overriding-header (sbw/org-config--title "Stalled tasks"))
                   (org-agenda-files ,files)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                   (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if 'scheduled 'deadline)))))
            (todo "DONE|CANCELLED"
                  ((org-agenda-overriding-header (sbw/org-config--title "Completed tasks"))
                   (org-agenda-files ,files)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                   (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if 'scheduled 'deadline)))))))))

(defmacro sbw/org-config-agenda (binding title days files)
  `(quote (,binding
           ,title
           ((agenda ""
                    ((org-agenda-ndays ,days)
                     (org-agenda-files ,files)))))))

(setq
 sbw/org-config-personal-files (sbw/org-config-files ["current"] ["personal"])
 sbw/org-config-work-files     (sbw/org-config-files ["current"] ["work"]))

(setq org-agenda-custom-commands nil)
(add-to-list 'org-agenda-custom-commands '("c" . "Custom agenda"))
(add-to-list 'org-agenda-custom-commands '("cp" . "Personal"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cpt" "Personal tasks" sbw/org-config-personal-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "cpa" "Personal agenda" 7 sbw/org-config-personal-files))
(add-to-list 'org-agenda-custom-commands '("cw" . "Work"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cwt" "Work tasks" sbw/org-config-work-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "cwa" "Work agenda" 7 sbw/org-config-work-files))
(add-to-list 'org-agenda-custom-commands '("cs" . "Selection"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cst" "Selection tasks" (sbw/ht-get sbw/org-config :selected-files)))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "csa" "selection agenda" 7 (sbw/ht-get sbw/org-config :selected-files)))
(add-to-list 'org-agenda-custom-commands '("ca" . "All"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cat" "All tasks" sbw/org-config-all-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "caa" "All agenda" 7 sbw/org-config-all-files))

(defun sbw/org-config-agenda-selected-agenda () (interactive) (org-agenda nil "csa"))
(defun sbw/org-config-agenda-selected-tasks  () (interactive) (org-agenda nil "cst"))
(defun sbw/org-config-agenda-work-agenda     () (interactive) (org-agenda nil "cwa"))
(defun sbw/org-config-agenda-work-tasks      () (interactive) (org-agenda nil "cwt"))
(defun sbw/org-config-agenda-personal-agenda () (interactive) (org-agenda nil "cpa"))
(defun sbw/org-config-agenda-personal-tasks  () (interactive) (org-agenda nil "cpt"))

;; Agenda appearance

(setq
 org-agenda-remove-tags   1
 org-agenda-prefix-format '( (agenda   . " %-20:c%-20t%-20s")
                             (timeline . "  %s")
                             (todo     . " %-20:c")
                             (tags     . " %-20:c")
                             (search   . " %-20:c")) )

;; Appointments
;; Refresh when the agenda is displayed

(defun sbw/org-refresh-appointments-from-agenda ()
  "Update the appointment list from the agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(add-hook 'org-finalize-agenda-hook 'sbw/org-refresh-appointments-from-agenda 'append)
(appt-activate t)

(provide 'sbw-org-config)
