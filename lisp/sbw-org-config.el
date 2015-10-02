;; General org-mode configuration

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

(defun sbw/org-config ()
  "Returns information about my org-mode configuration."
  (let* ( (org-file? (lambda (x) (s-ends-with? ".org" (f-filename x))))
          (all-files (f-entries org-directory org-file? :recursive)) )
    (sbw/ht-create
      :all-files   all-files
      :categorised (sbw/org-config--categorise-files all-files))))

(defun sbw/org-config-projects (config)
  "Returns a list of org-mode projects."
  (sbw/ht-keys (sbw/ht-get config :categorised)))

(defun sbw/org-config-categories (config)
  "Returns a list of org-mode categories."
  (seq-uniq
    (seq-mapcat
      'sbw/ht-keys
      (sbw/ht-vals (sbw/ht-get config :categorised))())))

(defun sbw/org-config-files (config projects categories)
  "Returns the org-mode files filtered by the specified PROJECTS
and CATEGORIES, where PROJECTS and CATEGORIES are lists of string
names, or nil to indicate that all should be included."
  (let* ( (projects    (or projects   (sbw/org-config-projects config)))
          (categories  (or categories (sbw/org-config-categories config)))
          (filter-vals (lambda (ht ks) (sbw/ht-vals (sbw/ht-select-keys ht ks)))) )
    (apply (-partial 'seq-concatenate 'list)
      (seq-mapcat
        (lambda (x) (funcall filter-vals x categories))
        (funcall filter-vals (sbw/ht-get config :categorised) projects)))))

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
 sbw/org-config-personal-files (sbw/org-config-files (sbw/org-config) ["current"] ["personal"])
 sbw/org-config-work-files     (sbw/org-config-files (sbw/org-config) ["current"] ["work"])
 sbw/org-config-level-up-files (sbw/org-config-files (sbw/org-config) ["current"] ["level-up"])
 sbw/org-config-all-files      (sbw/org-config-files (sbw/org-config) ["current"] nil))

(setq org-agenda-custom-commands nil)
(add-to-list 'org-agenda-custom-commands '("c" . "Custom agenda"))
(add-to-list 'org-agenda-custom-commands '("cp" . "Personal"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cpt" "Personal tasks" sbw/org-config-personal-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "cpa" "Personal agenda" 7 sbw/org-config-personal-files))
(add-to-list 'org-agenda-custom-commands '("cw" . "Work"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cwt" "Work tasks" sbw/org-config-work-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "cwa" "Work agenda" 7 sbw/org-config-work-files))
(add-to-list 'org-agenda-custom-commands '("cl" . "Level-up"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "clt" "Level-up tasks" sbw/org-config-level-up-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "cla" "Level-up agenda" 7 sbw/org-config-level-up-files))
(add-to-list 'org-agenda-custom-commands '("ca" . "All"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cat" "All tasks" sbw/org-config-all-files))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "caa" "All agenda" 7 sbw/org-config-all-files))

(sbw/org-config-categories (sbw/org-config))

;; Agenda appearance

(setq
 org-agenda-remove-tags   1
 org-agenda-prefix-format '( (agenda   . " %-20:c%-20t%s")
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

