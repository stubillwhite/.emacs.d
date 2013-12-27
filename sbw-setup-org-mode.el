;; TODO 
;;  - Remove duplication
;(require 'org)

;; Default to clean view with no leading asterisks for indentation
(setq-default org-startup-indented t)

;; Clock and log into drawers, include NOTES drawer
(setq org-clock-into-drawer t)
(setq org-log-into-drawer   t)

;; Tags and shortcut keys
(setq org-tag-alist '(("Bug" . ?b) ("Support" . ?s) ("Unplanned" . ?u)))

;; Capture directory
(setq org-directory 
      "c:/users/ibm_admin/my_local_stuff/home/my_stuff/srcs/org/")

;; Agenda files
(setq org-agenda-files (mapcar (lambda (x) (concat org-directory x)) 
                               (list "todo-personal.org" "todo-work.org" "timesheet.org" "incoming.org" "weekly-plan.org")))

(setq org-default-notes-file (concat org-directory "incoming.org") )

;; Key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Integrate with diary
;; (setq org-agenda-include-diary t)

;; Child items should inherit all from parents
(setq org-use-property-inheritance t)

;; Notes configuration


(setq org-protocol-default-template-key "l")
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "/path/to/notes.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("l" "Link" entry (file+olp "/path/to/notes.org" "Web Links")
        "* %a\n %?\n %i")
   ("j" "Journal" entry (file+datetree "/path/to/journal.org")
        "* %?\nEntered on %U\n  %i\n  %a")))

;; Archive as much information as possible
(setq org-archive-save-context-info
  '(time file ltags itags todo category olpath))

;; Display times as hours and minutes only
(setq org-time-clocksum-format 
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Not working :(
;;
;; (add-hook 'org-mode-hook
;;       '(lambda ()
;;          (delete '("\\.pdf\\'" . default) org-file-apps)
;;          (add-to-list 'org-file-apps '("\\.pdf\\'" . "explorer %s"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Right-align tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun sbw-org-right-align-agenda-tags ()
;;   "Right align the tags in agenda mode."
;;   (setq org-agenda-tags-column (- (window-width)))
;;   (org-agenda-align-tags)
;;   (redisplay t))
;; 
;; (add-hook 'org-finalize-agenda-hook 'sbw-org-right-align-agenda-tags)
;; 
;; (defun sbw-org-right-align-tags ()
;;   "Right align the tags in org mode."
;;   (let ((saved-buffer-modified-p (buffer-modified-p)))
;;     (setq org-tags-column (- (window-width)))
;;     (org-align-all-tags)
;;     (set-buffer-modified-p saved-buffer-modified-p)
;;     (redisplay t)))
;; 
;; (add-hook 'window-configuration-change-hook 'sbw-org-right-align-tags)
;; 
;; (defun sbw-org-left-align-tags-before-save ()
;;   "Left align the tags in org mode."
;;   (setq org-tags-column 1)
;;   (org-align-all-tags))
;; 
;; (add-hook 'before-save-hook 'sbw-org-left-align-tags-before-save)
;; 
;; (defun sbw-org-restore-tag-alignment-after-save ()
;;   "Restore tag alignment altered by before-save hook."
;;   (sbw-org-right-align-tags)
;;   (set-buffer-modified-p nil))
;; 
;; (add-hook 'after-save-hook 'sbw-org-restore-tag-alignment-after-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; (setq ba/org-adjust-tags-column t)
;; 
;; (defun ba/org-adjust-tags-column-reset-tags ()
;;   "In org-mode buffers it will reset tag position according to
;; `org-tags-column'."
;;   (when (and
;;          (not (string= (buffer-name) "*Remember*"))
;;          (eql major-mode 'org-mode))
;;     (let ((b-m-p (buffer-modified-p)))
;;       (condition-case nil
;;           (save-excursion
;;             (goto-char (point-min))
;;             (command-execute 'outline-next-visible-heading)
;;             ;; disable (message) that org-set-tags generates
;;             (flet ((message (&rest ignored) nil))
;;               (org-set-tags 1 t))
;;             (set-buffer-modified-p b-m-p))
;;         (error nil)))))
;; 
;; (defun ba/org-adjust-tags-column-now ()
;;   "Right-adjust `org-tags-column' value, then reset tag position."
;;   (set (make-local-variable 'org-tags-column)
;;        (- (- (window-width) (length org-ellipsis))))
;;   (ba/org-adjust-tags-column-reset-tags))
;; 
;; (defun ba/org-adjust-tags-column-maybe ()
;;   "If `ba/org-adjust-tags-column' is set to non-nil, adjust tags."
;;   (when ba/org-adjust-tags-column
;;     (ba/org-adjust-tags-column-now)))
;; 
;; (defun ba/org-adjust-tags-column-before-save ()
;;   "Tags need to be left-adjusted when saving."
;;   (when ba/org-adjust-tags-column
;;      (setq org-tags-column 1)
;;      (ba/org-adjust-tags-column-reset-tags)))
;; 
;; (defun ba/org-adjust-tags-column-after-save ()
;;   "Revert left-adjusted tag position done by before-save hook."
;;   (ba/org-adjust-tags-column-maybe)
;;   (set-buffer-modified-p nil))
;; 
;; ; automatically align tags on right-hand side
;; (add-hook 'window-configuration-change-hook
;;           'ba/org-adjust-tags-column-maybe)
;; (add-hook 'before-save-hook 'ba/org-adjust-tags-column-before-save)
;; (add-hook 'after-save-hook 'ba/org-adjust-tags-column-after-save)
;; (add-hook 'org-agenda-mode-hook '(lambda ()
;;                                   (setq org-agenda-tags-column (- (window-width)))))
;; 
;; ; between invoking org-refile and displaying the prompt (which
;; ; triggers window-configuration-change-hook) tags might adjust,
;; ; which invalidates the org-refile cache
;; (defadvice org-refile (around org-refile-disable-adjust-tags)
;;   "Disable dynamically adjusting tags"
;;   (let ((ba/org-adjust-tags-column nil))
;;     ad-do-it))
;; (ad-activate 'org-refile)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (server-start)
;; org-protocol://store-link://http:%2F%2Flocalhost%2Findex.html/The%20title

;;; ;; http://orgmode.org/worg/org-hacks.html
;;; ;;
;;; (defun my-org-inherited-no-file-tags ()
;;;   (let ((tags  (org-entry-get nil "ALLTAGS" 'selective))
;;;         (ltags (org-entry-get nil "TAGS")))
;;;     (mapc (lambda (tag)
;;;             (setq tags
;;;                   (replace-regexp-in-string (concat tag ":") "" tags)))
;;;           (append org-file-tags (when ltags (split-string ltags ":" t))))
;;;     (if (string= ":" tags) nil tags)))
;;; 
;;; (defadvice org-archive-subtree (around my-org-archive-subtree-low-level activate)
;;;   (let ((tags (my-org-inherited-no-file-tags))
;;;         (org-archive-location
;;;          (if (save-excursion (org-back-to-heading)
;;;                              (> (org-outline-level) 1))
;;;              (concat (car (split-string org-archive-location "::"))
;;;                      "::* "
;;;                      (car (org-get-outline-path)))
;;;            org-archive-location)))
;;;     ad-do-it
;;;     (with-current-buffer (find-file-noselect (org-extract-archive-file))
;;;       (save-excursion
;;;         (while (org-up-heading-safe))
;;;         (org-set-tags-to tags)))))
;;; 

;; Any headline with level <= 5 is a target
(setq org-refile-targets '((nil :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5)))


;; Provide refile targets as paths, including the file name
;; (without directory) as level 1 of the path
(setq org-refile-use-outline-path 'file)

;; Allow to create new nodes (must be confirmed by the user) as
;; refile targets
(setq org-refile-allow-creating-parent-nodes 'confirm)

(provide 'sbw-setup-org-mode)
