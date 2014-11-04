(require 'dash)
(require 'sbw-utils)
(require 'sbw-hash-tables)

(defun sbw/menu-action (key description function)
  "Returns a menu action with the specified KEY binding, DESCRIPTION, and FUNCTION to execute."
  (let* ( (table (sbw/ht-create)) )
    (puthash :key         key         table)
    (puthash :description description table)
    (puthash :function    function    table)
    table))

(defun sbw/-menu-add-action (menu action)
  "Adds ACTION to MENU."
  (puthash (gethash :key action) action menu)
  menu)

(defconst sbw/-menu-default-actions
  (let* ( (action-table (sbw/ht-create)) )
    (sbw/-menu-add-action action-table (sbw/menu-action 'f12 "Quit" (lambda () (message "Abort"))))
    action-table))

(defun sbw/menu (title &rest menu-actions)
  "Returns a menu with TITLE and the specified MENU-ACTIONS."
  (let* ( (menu-table   (sbw/ht-create))
          (action-table (-reduce-from 'sbw/-menu-add-action (copy-hash-table sbw/-menu-default-actions) menu-actions)) )
    (puthash :title   title        menu-table)
    (puthash :actions action-table menu-table)
    menu-table))

(defun sbw/-menu-key-to-string (key)
  "Returns the string representation of KEY."
  (if (symbolp key)
    (symbol-name key)
    (string key)))

(defun sbw/-menu-compare-keys (a b)
  "A comparator for keys."
  (let* ( (cmp (lambda (a b) (s-less? (sbw/-menu-key-to-string a) (sbw/-menu-key-to-string b)))) )
    (cond
      ((and (symbolp a) (symbolp b)) (funcall cmp a b))
      ((symbolp a)                   nil)
      ((symbolp b)                   t)
      (t                             (funcall cmp a b)))))

(defun sbw/-menu-format-action (action)
  "Returns the string to display for ACTION."
  (let* ( (key         (gethash :key action))
          (description (gethash :description action)) )
    (concat "[" (sbw/-menu-key-to-string key) "]  " description "\n")))

(defun sbw/-menu-format-actions (actions)
  "Returns the string to display for ACTIONS."
  (let* ( (keys (-sort 'sbw/-menu-compare-keys (sbw/ht-keys actions))) )
    (apply
      'concat (-map (lambda (x) (sbw/-menu-format-action (gethash x actions))) keys))))

(defun sbw/-menu-format-menu (menu)
  "Returns a string to display for MENU."
  (concat
    (sbw/heading-two (gethash :title menu))
    "\n"
    (sbw/-menu-format-actions (gethash :actions menu))
    "\n--\n"))

(defun sbw/-menu-execute-action (menu key)
  "Execute the action for KEY in MENU."
  (-if-let (action (gethash key (gethash :actions menu)))
    (funcall (gethash :function action))
    (message "Invalid key %c" key)))

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
    (setq key (read-key))
    (message ""))
  key)

(defun sbw/menu-display (menu)
  "Display MENU and allow the user to select and execute an action from it."
  (interactive)
  (sbw/-menu-execute-action menu (sbw/-menu-select-action menu)))

(defmacro sbw/menu-submenu (key description submenu)
  `(sbw/menu-action ,key ,description (lambda () (sbw/menu-display ,submenu))))

(defconst sbw/menu-reports
  (sbw/menu "Reports"
    (sbw/menu-action ?w "Weekly report for previous week"   'sbw/generate-weekly-report-for-previous-week)
    (sbw/menu-action ?m "Monthly report for previous month" 'sbw/generate-monthly-report-for-previous-month)
    (sbw/menu-action ?W "Weekly report for current week"    'sbw/generate-weekly-report-for-current-week)
    (sbw/menu-action ?M "Monthly report for current month"  'sbw/generate-monthly-report-for-current-month)))

(defconst sbw/menu-timers
  (sbw/menu "Timers"
    (sbw/menu-action ?p "Toggle pomodoro timer" 'sbw/pomodoro-timer-toggle)
    (sbw/menu-action ?s "Toggle summary timer"  'sbw/summarise-timer-toggle)
    ))

(defconst sbw/menu-common-commands
  (sbw/menu "Common Commands"
    (sbw/menu-action  ?a "Agenda"        'org-agenda)
    (sbw/menu-action  ?r "Reports"       (lambda () (sbw/menu-display sbw/menu-reports)))
    (sbw/menu-submenu ?c "Timers"        sbw/menu-timers)
    (sbw/menu-action ?t "Toggle pomodoro timer" 'sbw/pomodoro-timer-toggle)
    (sbw/menu-action ?s "Toggle summary timer"  'sbw/summarise-timer-toggle)
    (sbw/menu-action ?w "Work agenda"  (lambda () (interactive) (org-agenda nil "cw")))
    (sbw/menu-action ?p "Personal agenda"  (lambda () (interactive) (org-agenda nil "cp")))
    (sbw/menu-action  ?x "Export agenda" 'org-store-agenda-views)))

(provide 'sbw-menu)
