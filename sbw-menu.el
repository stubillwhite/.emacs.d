(require 'dash)
(require 'sbw-utils)
(require 'sbw-hash-table-utils)

(defun sbw/menu-option (key description function)
  "Returns a menu option with the specified KEY binding, DESCRIPTION, and FUNCTION to execute."
  (let* ( (table (sbw/ht-create)) )
    (puthash :key         key         table)
    (puthash :description description table)
    (puthash :function    function    table)
    table))

(defun sbw/menu-add-option (menu option)
  (puthash (gethash :key option) option menu)
  menu)

(defconst sbw/menu-default-options
  (let* ( (table (sbw/ht-create)) )
    (sbw/menu-add-option table (sbw/menu-option ?q "Quit" (lambda () (message "Abort"))))
    table))

(defun sbw/menu (title &rest menu-options)
  "Returns a menu with TITLE and the specified MENU-OPTIONS."
  (let* ( (menu-table   (sbw/ht-create))
          (option-table (-reduce-from 'sbw/menu-add-option (copy-hash-table sbw/menu-default-options) menu-options)) )
    (puthash :title   title        menu-table)
    (puthash :options option-table menu-table)
    menu-table))

(defun sbw/menu-format-option (option)
  (let* ( (key         (gethash :key option))
          (description (gethash :description option)) )
    (concat (string key) "  " description "\n")))

(defun sbw/menu-format-options (options)
  (let* ( (keys (-sort '< (sbw/ht-keys options))) )
    (apply
      'concat (-map (lambda (x) (sbw/menu-format-option (gethash x options))) keys))))

(defun sbw/menu-format-menu (menu)
  (concat
    (sbw/heading-two (gethash :title menu))
    (sbw/menu-format-options (gethash :options menu))))

(defun sbw/menu-execute-option (menu key)
  (-if-let (option (gethash key (gethash :options menu)))
    (funcall (gethash :function option))
    (message "Invalid key %c" key)))

(defun sbw/menu-select-option (menu)
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer-other-window "*Menu*")
    (erase-buffer)
    (insert (sbw/menu-format-menu menu))
    ;(fit-frame-to-buffer (current-buffer))
    (message "Press key for command")
    (setq key (read-char-exclusive))
    (message ""))
  key)

(defun sbw/menu-display (menu)
  "Display MENU and allow the user to select and execute an option from it."
  (interactive)
  (sbw/menu-execute-option menu (sbw/menu-select-option menu)))

(defun sbw/menu-submenu (submenu)
  (sbw/menu-display submenu))

(defconst sbw/menu-reports
  (sbw/menu "Reports"
    (sbw/menu-option ?w "Weekly report"  'sbw/generate-weekly-report)
    (sbw/menu-option ?m "Monthly report" 'sbw/generate-monthly-report)))

(defconst sbw/menu-common-commands
  (sbw/menu "Common Commands"
    (sbw/menu-option ?a "Agenda"        'org-agenda)
    (sbw/menu-option ?r "Reports"       (lambda () (sbw/menu-display sbw/menu-reports)))
    (sbw/menu-option ?x "Export agenda" 'org-store-agenda-views)))

(provide 'sbw-menu)
