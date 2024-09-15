;;; sbw/org-config.el --- Functions for managing my org-mode configuration

(require 's)
(require 'f)
(require 'dash)

;; Functions for managing files within my org-mode file structure, which is of the form:
;; 
;;     org-directory
;;       workflow
;;         category
;;           project.org
;;
;; From bottom up:
;;  - Individual org files track things for a particular project (e.g., a holiday, building something)
;;  - Categories group related projects (e.g., projects at work, domestic projects, recreational coding projects)
;;  - Workflows group categories into flows (e.g., active stuff, inactive stuff, archived stuff)
;;
;; This could all probably be done with one monolithic file and tags, but I like to keep stuff separate and my org files
;; get large and slow due to notes in tasks, code snippets, logs. Particularly on Windows.
;;
;; Project tasks are archived to a hardcoded "archive" workflow to move them out of the way.

(defun sbw/org-config-project-attributes (path)
  "Returns a hash-map of the attributes of the project file at PATH."
  (let* ( (split-path (s-split "/" (f-relative path org-directory)))
          (workflow   (car split-path))
          (category   (cadr split-path))
          (project    (caddr split-path)) )
    (sbw/ht-create :workflow workflow :category category :project project)))

(defun sbw/org-config--build-index (projects)
  (-reduce-from
    (lambda (acc x)
      (let* ( (attributes (sbw/org-config-project-attributes x))
              (workflow   (sbw/ht-get attributes :workflow))
              (category   (sbw/ht-get attributes :category)) )
        (sbw/ht-update-in acc (vector workflow category) (lambda (y) (cons x y)))))
    (sbw/ht-create)
    projects))

(defun sbw/org-config--find-and-index-projects ()
  (let* ( (org-file?    (lambda (x) (s-ends-with? ".org" (f-filename x))))
          (all-projects (f-entries org-directory org-file? :recursive))
          (index        (sbw/org-config--build-index all-projects)) )
    (sbw/ht-create
      :all-projects        all-projects
      :index               index
      :selected-workflows  nil
      :selected-categories nil
      :selected-projects   all-projects)))

(defun sbw/org-config--set-org-variables ()
  (setq
   org-agenda-files       (sbw/ht-get sbw/org-config :selected-projects)
   org-default-notes-file (concat org-directory "/incoming.org")
   org-refile-targets     (quote ((org-agenda-files :maxlevel . 1))))
  sbw/org-config)

(defun sbw/org-config-refresh ()
  "Refresh the org-mode configuration."
  (interactive)
  (setq sbw/org-config (sbw/org-config--find-and-index-projects))
  (sbw/org-config--set-org-variables))

(defvar sbw/org-config (sbw/org-config-refresh)
   "The org-mode configuration.")

(defun sbw/org-config-workflows ()
  "Returns a list of org-mode workflows."
  (sbw/ht-keys (sbw/ht-get sbw/org-config :index)))

(defun sbw/org-config-categories ()
  "Returns a list of org-mode categories."
  (seq-uniq
    (seq-mapcat
      'sbw/ht-keys
      (sbw/ht-vals (sbw/ht-get sbw/org-config :index)))))

(defun sbw/org-config-projects (workflows categories)
  "Returns the projects filtered by the specified WORKFLOWS and
CATEGORIES, where WORKFLOWS and CATEGORIES are lists of string
names, or nil to indicate that all should be included."
  (let* ( (workflows   (or workflows  (sbw/org-config-workflows)))
          (categories  (or categories (sbw/org-config-categories)))
          (filter-vals (lambda (ht ks) (sbw/ht-vals (sbw/ht-select-keys ht ks)))) )
    (apply (-partial 'seq-concatenate 'list)
      (seq-mapcat
        (lambda (x) (funcall filter-vals x categories))
        (funcall filter-vals (sbw/ht-get sbw/org-config :index) workflows)))))

(defun sbw/org-config--select-workflows-and-categories ()
  (let* ( (workflows-source  (helm-build-sync-source "Workflows"
                               :candidates (sbw/org-config-workflows)
                               :action     (lambda (candidate) (helm-marked-candidates))))
          (categories-source (helm-build-sync-source "Categories"
                               :candidates (sbw/org-config-categories)
                               :action     (lambda (candidate) (helm-marked-candidates)))) )
    (list
     (helm :buffer "*helm org-config*" :sources workflows-source)
     (helm :buffer "*helm org-config*" :sources categories-source))))

(defun sbw/org-config-projects-by-name (names)
  "Returns a list of projects whose name is present in NAMES"
  (->> (sbw/org-config-projects nil nil)
       (-filter (lambda (x) (seq-position names (sbw/ht-get (sbw/org-config-project-attributes x) :project))))))

(defun sbw/org-config-projects-with-flag (flag)
  "Returns a list of projects which have the property flag FLAG"
  (let* ((get-flags (lambda (x) (save-excursion
                             (set-buffer (find-file-noselect x))
                             (goto-char 0)
                             (or (sbw/org-get-property "FLAGS") "")))))
    (->> (sbw/org-config-projects nil nil)
         (-filter (lambda (x) (s-contains? flag (funcall get-flags x))))))  )

(defun sbw/org-config-select (workflows categories)
  "Select the org-mode files filtered by the specified WORKFLOWS
and CATEGORIES, where WORKFLOWS and CATEGORIES are lists of
string names, or nil to indicate that all should be included. If
called interactively, prompt to select WORKFLOWS and CATEGORIES."
  (interactive (sbw/org-config--select-workflows-and-categories))
  (setq sbw/org-config (-> sbw/org-config
                           (sbw/ht-assoc :selected-workflows workflows)
                           (sbw/ht-assoc :selected-categories categories)
                           (sbw/ht-assoc :selected-projects (sbw/org-config-projects workflows categories))))
  (sbw/org-config--set-org-variables))

(defun sbw/org-config-default ()
  "Select default configuration, which includes all current projects and excludes all non-project files."
  (interactive)
  (sbw/org-config-select ["current"] (-filter (lambda (x) (not (seq-contains ["non-project"] x))) (sbw/org-config-categories))))

(defmacro sbw/org-config-with-selection (workflows categories &rest body)
  "Execute BODY with temporary selection of projects."
  `(let* ( (original-workflows  (sbw/ht-get sbw/org-config :selected-workflows))
           (original-categories (sbw/ht-get sbw/org-config :selected-categories)) )
     (progn
       (sbw/org-config-select ,workflows ,categories))
     ,@body
     (progn
       (sbw/org-config-select original-workflows original-categories))))

(defun sbw/org-config-new-file ()
  "Create a new file with prompted WORKFLOW, CATEGORY, and PROJECT"
  (interactive)
  (let* ( (workflow (completing-read "Workflow: " (sbw/org-config-workflows)))
          (category (completing-read "Category: " (sbw/org-config-categories)))
          (project  (completing-read "Project: "  (-map (lambda (f) (f-no-ext (f-filename f))) (sbw/org-config-projects (list workflow) (list category)))))
          (content  (f-read-text (s-lex-format "${sbw/lisp-path}/sbw-org-config-new-file-template.org")))
          (path     (s-lex-format "${org-directory}/${workflow}/${category}/${project}.org")) )
    (when (not (f-exists? path))
      (apply 'f-mkdir (f-split (f-dirname path)))
      (f-write (->> content
                    (s-replace-all `(("${category}" . ,project)))) 'utf-8 path)
      (sbw/org-config-refresh)
      (message "Created and added %s" path))))

;; Agenda

(defun sbw/org-config--title (title)
  (concat "\n" title "\n" (make-string (length title) ?-) "\n"))

(defmacro sbw/org-config-black-prioritised-tasks (binding title files)
  `(quote (,binding
           ,title
           ((tags-todo "TAGS=\":today:\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Tags - today"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))))))))

(defmacro sbw/org-config--agenda-section (title pattern files)
  `(quote (tags-todo ,pattern
                     ((org-agenda-overriding-header (sbw/org-config--title ,title))
                      (org-agenda-files ,files)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))))))

(defmacro sbw/org-config-white-prioritised-tasks (binding title files)
  `(quote (,binding
           ,title
           (,(sbw/org-config--agenda-section "New priorities for today" "PRIORITY=\"A\"" ,files)))))

;; (macroexp--expand-all '(sbw/org-config--agenda-section "New priorities for today" "PRIORITY=\"A\"" ,files))
;; (macroexpand-all (sbw/org-config-white-prioritised-tasks "binding" "title" "my-files"))
;; (macroexpand-all (sbw/org-config-black-prioritised-tasks "binding" "title" "my-files"))

(defun sbw/org-config--heading-tags ()
  (org-no-properties (org-entry-get (point) "TAGS")))

(defun sbw/skip-if-has-tags ()
  (let* ((subtree nil)
         (end     (if subtree (save-excursion (org-end-of-subtree t) (point)) (org-entry-end-position))))
    (if (s-blank-str? (sbw/org-config--heading-tags)) nil end)))

(defun sbw/skip-if-categorised ()
  (or (org-agenda-skip-entry-if 'scheduled 'deadline) (sbw/skip-if-has-tags)))

(defmacro sbw/org-config-prioritised-tasks (binding title files)
  `(quote (,binding
           ,title
           ((agenda ""
                    ((org-agenda-overriding-header (sbw/org-config--title "Schedule for the day"))
                     (org-agenda-span 1)
                     (org-agenda-files ,files)))
            ;; (tags-todo "MATRIX=\"\""
            ;;            ((org-agenda-overriding-header (sbw/org-config--title "WHITE"))
            ;;             (org-agenda-files ,files)
            ;;             (org-agenda-todo-ignore-scheduled t)
            ;;             (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
            ;;             ;; (org-agenda-skip-function (lambda ()
            ;;             ;;                             (or (org-agenda-skip-entry-if 'scheduled 'deadline)
            ;;             ;;                                 (sbw/skip-if-has-tags))))
            ;;             (org-agenda-skip-function 'sbw/skip-if-categorised)
            ;;             ))
            (todo "BLOCKED"
                  ((org-agenda-overriding-header (sbw/org-config--title "Waiting on others"))
                   (org-agenda-files ,files)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                   (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if 'scheduled 'deadline)))))
            (tags-todo "TAGS=\":catchup:\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Tags - catchup"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))                        
                        ))
            (tags-todo "TAGS=\":today:\"-TODO=\"BLOCKED\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Tags - today"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))                        
                        ))
            (tags-todo "TAGS=\":thisWeek:\"-TODO=\"BLOCKED\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Tags - thisWeek"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        ))
            (tags-todo "TAGS=\":nextWeek:\"-TODO=\"BLOCKED\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Tags - nextWeek"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        ))
            (tags-todo "TAGS=\":admin:\"-TODO=\"BLOCKED\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Tags - admin"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        ))
            (agenda ""
                    ((org-agenda-overriding-header (sbw/org-config--title "Schedule for the week"))
                     (org-agenda-span 7)
                     (org-agenda-files ,files)))
            (tags-todo "MATRIX=\"urgent-important\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Priority tasks - Do it"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        (org-agenda-skip-function 'sbw/skip-if-categorised)
                        ))
            (tags-todo "MATRIX=\"not-urgent-important\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Priority tasks - Schedule it"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        (org-agenda-skip-function 'sbw/skip-if-categorised)
                        ))
            (tags-todo "MATRIX=\"urgent-not-important\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Priority tasks - Delegate it"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        (org-agenda-skip-function 'sbw/skip-if-categorised)
                        ))
            (tags-todo "MATRIX=\"not-urgent-not-important\""
                       ((org-agenda-overriding-header (sbw/org-config--title "Priority tasks - De-clutter it"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        (org-agenda-skip-function 'sbw/skip-if-categorised)
                        ))
            (tags-todo "MATRIX=\"\""
                       ((org-agenda-overriding-header (sbw/org-config--title "To prioritise"))
                        (org-agenda-files ,files)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                        (org-agenda-skip-function 'sbw/skip-if-categorised)
                        ))
            (todo "DONE|CANCELLED"
                  ((org-agenda-overriding-header (sbw/org-config--title "Completed tasks"))
                   (org-agenda-files ,files)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down category-up alpha-up))
                   (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if 'scheduled 'deadline)))))
            ))))

(defmacro sbw/org-config-agenda (binding title days files)
  `(quote (,binding
           ,title
           ((agenda ""
                    ((org-agenda-ndays ,days)
                     (org-agenda-files ,files)))))))

(setq org-agenda-custom-commands nil)
(add-to-list 'org-agenda-custom-commands '("c" . "Custom agenda"))

(add-to-list 'org-agenda-custom-commands '("cp" . "Personal"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cpt" "Personal tasks" (sbw/org-config-projects ["current"] ["kd"])))

(add-to-list 'org-agenda-custom-commands '("cw" . "Work"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cwt" "Work tasks" (sbw/org-config-projects ["current"] ["work"])))

(add-to-list 'org-agenda-custom-commands '("ck" . "KD"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "ckt" "KD tasks" (sbw/org-config-projects ["current"] ["kd"])))

(add-to-list 'org-agenda-custom-commands '("iw" . "Incubating"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "iwt" "Work tasks" (sbw/org-config-projects ["incubating"] ["work"])))

(add-to-list 'org-agenda-custom-commands '("cs" . "Selection"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cst" "Selection tasks" (sbw/ht-get sbw/org-config :selected-projects)))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "csa" "selection agenda" 7 (sbw/ht-get sbw/org-config :selected-projects)))

(add-to-list 'org-agenda-custom-commands '("cf" . "Focus area"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cft" "Focus area tasks" (sbw/org-config-projects-with-flag "focus-area")))

(add-to-list 'org-agenda-custom-commands '("cd" . "Domain"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cdt" "Domain tasks" (sbw/org-config-projects-with-flag "domain")))

(add-to-list 'org-agenda-custom-commands '("cm" . "Management"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cmt" "Management tasks" (sbw/org-config-projects-with-flag "management")))

(add-to-list 'org-agenda-custom-commands '("ca" . "All"))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-prioritised-tasks "cat" "All tasks" sbw/org-config-all-projects))
(add-to-list 'org-agenda-custom-commands (sbw/org-config-agenda            "caa" "All agenda" 7 sbw/org-config-all-projects))

(defun sbw/org-config--open-agenda (agenda)
  (org-agenda nil agenda)
  (delete-other-windows))

(defun sbw/org-config-agenda-selected-agenda   () (interactive) (sbw/org-config--open-agenda "csa"))
(defun sbw/org-config-agenda-selected-tasks    () (interactive) (sbw/org-config--open-agenda "cst"))

(defun sbw/org-config-agenda-work-tasks        () (interactive) (sbw/org-config--open-agenda "cwt"))
(defun sbw/org-config-agenda-kd-tasks          () (interactive) (sbw/org-config--open-agenda "ckt"))
(defun sbw/org-config-agenda-personal-tasks    () (interactive) (sbw/org-config--open-agenda "cpt"))
(defun sbw/org-config-agenda-focus-area-tasks  () (interactive) (sbw/org-config--open-agenda "cft"))
(defun sbw/org-config-agenda-domain-tasks      () (interactive) (sbw/org-config--open-agenda "cdt"))
(defun sbw/org-config-agenda-management-tasks  () (interactive) (sbw/org-config--open-agenda "cmt"))

(defun sbw/org-config-agenda-incubating-work-tasks  () (interactive) (sbw/org-config--open-agenda "iwt"))

;; Agenda appearance

(setq
 org-agenda-remove-tags   1
 org-agenda-prefix-format '( (agenda   . " %-25:c%-20t%-20s")
                             (timeline . "  %s")
                             (todo     . " %-25:c")
                             (tags     . " %-25:c")
                             (search   . " %-25:c")) )

;; Refresh appointments when the agenda is displayed

(defun sbw/org-config--refresh-appointments-from-agenda ()
  (message "Updating appointments from agenda")
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(add-hook 'org-finalize-agenda-hook 'sbw/org-config--refresh-appointments-from-agenda 'append)
(appt-activate t)

;; Strip tags when closing tasks

(defun sbw/org-config--remove-tags-when-done ()
  (interactive)
  (let* ((state (org-no-properties (org-get-todo-state))))
    (when (or (s-equals? state "DONE")
              (s-equals? state "CANCELLED"))
      (org-set-tags '()))))

(add-hook 'org-after-todo-state-change-hook 'sbw/org-config--remove-tags-when-done)

;; Archiving

(defun sbw/org-config--refile-immediate ()
  (let* ( (command            (vconcat [?\M-x] (string-to-vector "org-refile") [return] [return]))
          (org-refile-history '()) )
    (execute-kbd-macro command)))

(defun sbw/org-config--strip-suffix (suffix s)
  (if (s-ends-with? suffix s)
      (s-left (- (length s) (length suffix)) s)
    s))

(defun sbw/org-config-archive-task ()
  "Archive the task at point."
  (interactive)
  (let* ( (summary          (sbw/org-utils-heading-summary-at-point (point)))
          (attributes       (sbw/org-config-project-attributes buffer-file-name))
          (category         (sbw/ht-get attributes :category))
          (project          (sbw/ht-get attributes :project))
          (project-name     (sbw/org-config--strip-suffix ".org" project))
          (path             (s-lex-format "${org-directory}/archive/${category}/archive-${project}"))
          (original-targets org-refile-targets)
          (archive-targets  `(((,path) :maxlevel . 1))) )
    (when (not (f-file? path))
      (sbw/org-config-new-file "archive" category (s-concat "archive-" project-name)))
    (setq org-refile-targets archive-targets)
    (sbw/org-config--refile-immediate)
    (setq org-refile-targets original-targets)
    (message (format "Refiled to '%s'" path))))

;; Link handling -- personal links in personal browser, others in generic

(defun sbw/org-config-browse-url--chromium (url)
  (shell-command
   (s-concat "\"C:/Program Files (x86)/Google/Chrome/Application/chrome.exe\" " url)))

(defun sbw/org-config-browse-url--personal (url &optional NEW-WINDOW)
  (message "Opening in personal browser")
  (if (sbw/is-windows?)
      (sbw/org-config-browse-url--chromium url)
    (browse-url-chromium url NEW-WINDOW)))

(defun sbw/org-config-browse-url--default (url &optional NEW-WINDOW)
  (message "Opening in generic browser")
  (browse-url-default-browser url NEW-WINDOW))

(defun sbw/org-config-browse-url (url &optional NEW-WINDOW)
  (interactive)
  (let ( (attributes (sbw/org-config-project-attributes (buffer-file-name))) )
    (if (s-equals? (sbw/ht-get attributes :category) "personal")
        (sbw/org-config-browse-url--personal url NEW-WINDOW)
      (sbw/org-config-browse-url--default url NEW-WINDOW))))

(setq browse-url-browser-function 'sbw/org-config-browse-url)
(setq browse-url-browser-function 'browse-url-default-browser)

;; Default configuration initially

(sbw/org-config-refresh)
(sbw/org-config-default)

(provide 'sbw-org-config)

