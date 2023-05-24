;;; sbw/menu.el --- Functions for a simple menu of common actions

;; TODO: This needs refactoring or replacing with a hydra

(require 'dash)
(require 'sbw-utils)
(require 'sbw-hash-tables)
(require 'sbw-org-config)
(require 'sbw-org-tech-radar)

(defun sbw/menu-action (key description function)
  "Returns a menu action with the specified KEY binding, DESCRIPTION, and FUNCTION to execute."
  (sbw/ht-create
    :key         key
    :description description
    :function    function))

(defun sbw/-menu-add-action (menu action)
  "Adds ACTION to MENU."
  (sbw/ht-assoc menu (sbw/ht-get action :key) action))

(defconst sbw/-menu-default-actions
  (let* ( (action-table (sbw/ht-create)) )
    (sbw/-menu-add-action action-table (sbw/menu-action 'f12 "Quit" (lambda () (message "Abort"))))
    action-table))

(defun sbw/menu (title &rest menu-actions)
  "Returns a menu with TITLE and the specified MENU-ACTIONS."
  (-> (sbw/ht-create
        :title   title
        :actions (-reduce-from 'sbw/-menu-add-action (copy-hash-table sbw/-menu-default-actions) menu-actions))))

(defun sbw/-menu-key-to-string (key)
  "Returns the string representation of KEY."
  (if (symbolp key)
    (symbol-name key)
    (string key)))

(defun sbw/-menu-compare-keys (a b)
  "A comparator for keys."
  (let* ( (cmp (lambda (a b) (s-less? (sbw/-menu-key-to-string a) (sbw/-menu-key-to-string b)))) )
    (cond
      ((and (symbolp a) (symbolp b)) (funcall cmp b a))
      ((symbolp a)                   nil)
      ((symbolp b)                   t)
      (t                             (funcall cmp a b)))))

(defun sbw/-menu-format-action (action)
  "Returns the string to display for ACTION."
  (let* ( (key         (sbw/ht-get action :key))
          (description (sbw/ht-get action :description)) )
    (format "%5s  %s\n" (format "[%s]" (sbw/-menu-key-to-string key)) description )))

(defun sbw/-menu-format-actions (actions)
  "Returns the string to display for ACTIONS."
  (let* ( (keys (-sort 'sbw/-menu-compare-keys (sbw/ht-keys actions))) )
    (apply
      'concat (-map (lambda (x) (sbw/-menu-format-action (sbw/ht-get actions x))) keys))))

(defun sbw/-menu-format-heading (s)
  "Returns string S formatted to be a sub-heading."
  (concat s "\n" (make-string (length s) ?-) "\n"))

(defun sbw/-menu-format-menu (menu)
  "Returns a string to display for MENU."
  (concat
    (sbw/-menu-format-heading (sbw/ht-get menu :title))
    "\n"
    (sbw/-menu-format-actions (sbw/ht-get menu :actions))
    "\n--\n"))

(defun sbw/-menu-execute-action (menu key)
  "Execute the action for KEY in MENU."
  (-if-let  (action      (sbw/ht-get (sbw/ht-get menu :actions) key))
    (progn
      (let* ( (description (sbw/ht-get action :description))
              (function    (sbw/ht-get action :function)))
        (message description)
        (funcall function)))
    (progn
      (message "Invalid key %c" key))))

(defun sbw/-menu-select-action (menu)
  "Prompt the user to select an action from MENU."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer-other-window "*Menu*")
    (erase-buffer)
    (insert (sbw/-menu-format-menu menu))
    (fit-window-to-buffer)
    (goto-char (point-min))
    (message "Press key for command")
    (read-key)))

(defun sbw/menu-display (menu)
  "Display MENU and allow the user to select and execute an action from it."
  (interactive)
  (sbw/-menu-execute-action menu (sbw/-menu-select-action menu)))

(defmacro sbw/menu-submenu (key description submenu)
  "Returns a menu action to display a submenu, with the specified KEY binding, DESCRIPTION, and SUBMENU to display."
  `(sbw/menu-action ,key ,description (lambda () (sbw/menu-display ,submenu))))

(defconst sbw/menu-common-commands
  (sbw/menu "Common actions"
            (sbw/menu-submenu ?d "Dashboard"
                      (sbw/menu "Dashboard"
                                (sbw/menu-submenu ?s "Selection"
                                          (sbw/menu "Selection"
                                                    (sbw/menu-action ?c "Change"  (lambda () (interactive) (call-interactively 'sbw/org-config-select)))
                                                    (sbw/menu-action ?d "Default" 'sbw/org-config-default)
                                                    (sbw/menu-action ?a "Agenda"  'sbw/org-config-agenda-selected-agenda)
                                                    (sbw/menu-action ?t "Tasks"   'sbw/org-config-agenda-selected-tasks)))
                                (sbw/menu-action ?p "Personal"   'sbw/org-config-agenda-personal-tasks)
                                (sbw/menu-action ?w "Work"       'sbw/org-config-agenda-work-tasks)
                                (sbw/menu-action ?k "KD"         'sbw/org-config-agenda-kd-tasks)
                                (sbw/menu-action ?f "Focus area" 'sbw/org-config-agenda-focus-area-tasks)
                                (sbw/menu-action ?d "Domain"     'sbw/org-config-agenda-domain-tasks)
                                (sbw/menu-action ?m "Management" 'sbw/org-config-agenda-management-tasks)
                                ))
            (sbw/menu-action ?f "Refresh" 'sbw/org-config-refresh)
            (sbw/menu-submenu ?r "Review"
                      (sbw/menu "Review"
                                (sbw/menu-action ?w "Weekly report"  (lambda () (sbw/org-review-generate (sbw/org-review-config-for-weekly-report (current-time)))))
                                (sbw/menu-action ?m "Monthly report" (lambda () (sbw/org-review-generate (sbw/org-review-config-for-monthly-report (current-time)))))
                                (sbw/menu-action ?p "Period report"  (lambda () (sbw/org-review-generate (sbw/org-review-config-for-period))))
                                (sbw/menu-action ?s "Sprint report"  (lambda () (sbw/org-review-generate (sbw/org-review-config-for-sprint-report (current-time)))))
                                ))
            (sbw/menu-submenu ?t "Timers"
                      (sbw/menu "Timers"
                                (sbw/menu-action ?p "Toggle pomodoro timer" 'sbw/pomodoro-timer-toggle)
                                (sbw/menu-action ?s "Toggle summary timer"  'sbw/summarise-timer-toggle)))
            (sbw/menu-submenu ?x "Tech radar"
                      (sbw/menu "Tech radar"
                                (sbw/menu-action ?e "Edit tech radar"              (lambda () (find-file (sbw/dropbox-subfolder "Private/org/current/work/tech-radar.org"))))
                                (sbw/menu-action ?g "Generate and open tech radar" 'sbw/org-tech-radar-regenerate-and-open)))
            (sbw/menu-action ?z "Zsh" '(lambda () (interactive) (ansi-term "zsh")))))

(provide 'sbw-menu)
