;; Default to clean view with no leading asterisks for indentation
(setq-default org-startup-indented t)

;; Tags and shortcut keys
(setq org-tag-alist '(("Bug" . ?b) ("Support" . ?s) ("Unplanned" . ?u)))

;; Org directory
(setq org-directory 
      "c:/users/ibm_admin/my_local_stuff/home/my_stuff/srcs/org/")

;; Agenda files
(setq org-agenda-files (mapcar (lambda (x) (concat org-directory x)) 
                               (list "todo-personal.org" "todo-work.org" "timesheet.org" "incoming.org" "weekly-plan.org")))
(setq org-default-notes-file (concat org-directory "incoming.org") )

(setq 
  org-clock-into-drawer         t           ;; Clock into drawers
  org-ellipsis                  "\u2026"    ;; Small ellipsis character
  org-indent-mode               t           ;; Use indent mode
  org-log-into-drawer           t           ;; Log into drawers
  org-M-RET-may-split-line      nil         ;; Don't split lines
  org-replace-disputed-keys     t           ;; Prevent org-mode from binding shift-cursor keys
  org-return-follows-link       t           ;; Easy link navigation
  org-use-property-inheritance  t           ;; Child items should inherit all from parents
)

(defun sbw-org-mode/clock-in-switch-to-started (state) 
  "When clocking in, switch the task state STARTED if it is still in TODO." 
  (if (and (string-equal state "TODO") 
           (not (string-equal (buffer-name) "*Remember*"))) 
    "STARTED" 
    nil))

(setq org-clock-in-switch-to-state (quote sbw-org-mode/clock-in-switch-to-started))

;; Tag alignment
;; Ensure new tags are created right-aligned based on the window size, and
;; provide a handy function to re-align all tags in the buffer

(defun sbw-org-mode-set-org-tags-column-based-on-window-size ()
  "Set org-tags-column to right-align based on window size. Assumes that org-ellipsis is a string."
  (setq org-tags-column (- (- (window-width) (length org-ellipsis)))))

(add-hook 'window-configuration-change-hook 'sbw-org-mode-set-org-tags-column-based-on-window-size)

(defun sbw-org-mode-right-align-tags ()
  "Right-align the all tags in the buffer."
  (interactive)
  (sbw-org-mode-set-org-tags-column-based-on-window-size)
  (org-align-all-tags)
  (redisplay t))

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
