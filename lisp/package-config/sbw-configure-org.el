(require 'use-package)

(use-package org
  :init
  (progn
    (require 'org-clock)
    (require 'sbw-org-utils)
    
    ;; Default to clean view with no leading asterisks for indentation
    (setq-default org-startup-indented t)

    ;; Org files

    (setq org-directory 
      (cond
        ((eq system-type 'gnu/linux)  "~/Dropbox/Private/org")
        ((eq system-type 'windows-nt) "C:/Users/IBM_ADMIN/Dropbox/Private/org")
        ((eq system-type 'cygwin)     "/cygdrive/c/Users/IBM_ADMIN/Dropbox/Private/org")))
           
    (defun sbw/org-files (&rest dirs)
      "Return a list of the org files in directories DIRS."
      (-mapcat
        (lambda (x) (directory-files x :absolute org-agenda-file-regexp))
        (apply 'list dirs)))

    (defun sbw/org-find-org-files ()
      "Scan org-directory for org files."
      (interactive)
      (setq sbw/org-personal-files    (sbw/org-files (concat org-directory "/current/personal")))
      (setq sbw/org-non-project-files (sbw/org-files (concat org-directory "/current/non-project")))
      (setq sbw/org-work-files        (sbw/org-files (concat org-directory "/current/work")))
      (setq sbw/org-all-files         (append sbw/org-personal-files sbw/org-work-files sbw/org-non-project-files (list)))
      (setq sbw/org-refile-targets    (-filter (lambda (x) (not (-contains? sbw/org-non-project-files x))) sbw/org-all-files))
      (setq org-agenda-files          sbw/org-all-files)
      (setq org-default-notes-file    (concat org-directory "/current/non-project/incoming.org"))
      (setq org-refile-targets        (quote ((sbw/org-refile-targets :maxlevel . 1)))))

    (sbw/org-find-org-files)
    
    ;; General settings

    (setq 
      org-clock-into-drawer         t                      ;; Clock into drawers
      org-src-fontify-natively      t                      ;; Fontify embedded code blocks
      org-ellipsis                  "\u2026"               ;; Small ellipsis character
      org-agenda-fontify-priorities nil                    ;; Don't let priority change task representation
      org-indent-mode               t                      ;; Use indent mode
      org-log-into-drawer           t                      ;; Log into drawers
      org-log-done                  'time                  ;; Timestamp task completion so it can be used in reports
      org-M-RET-may-split-line      nil                    ;; Don't split lines
      org-return-follows-link       t                      ;; Easy link navigation
      org-use-property-inheritance  t                      ;; Child items should inherit all from parents
      org-default-priority          ?B                     ;; Default priority for unprioritised items
      appt-display-interval         5                      ;; Reminder for an appointment every five minutes...
      appt-message-warning-time     15                     ;; ...starting fifteeen minutes before it is due
      org-frame-title-format-backup sbw/frame-title-format ;; Override title frame title format
      org-tag-alist                 nil                    ;; No tags
      org-startup-folded            'content               ;; Display content when first opening org files
      )
    
    (setq org-todo-keywords
      '("TODO(t)" "STARTED(s)" "BLOCKED(b)" "POSTPONED(p)" "|" "DONE(d!)" "CANCELLED(c)"))

    (setq org-drawers
      '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES"))

    (setq org-archive-save-context-info
      '(time file ltags itags todo category olpath))

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

    (setq org-clock-in-switch-to-state (quote sbw/clock-in-switch-to-started)))

  :config
  (progn
    ;; Link type for opening a file in a running Eclipse instance
    (org-add-link-type "eclipse"
      (lambda (path)
        (start-process-shell-command "Eclipse" nil (concat "\"c:\\Program Files\\DevComponents\\Eclipse\\eclipse.exe\" --launcher.openFile " path))))

    ;; Link type for opening a file in Vim
    (org-add-link-type "vim"
      (lambda (path)
        (start-process-shell-command "Vim" nil (concat "gvim.exe " path))))

    ;; Link type for opening a file in Atom
    (org-add-link-type "atom"
      (lambda (path)
        (start-process-shell-command "Atom" nil (concat "C:\\Users\\IBM_ADMIN\\my_local_stuff\\home\\utils\\bin\\atom-windows\\Atom\\Atom.exe " path))))
         
    ;; TODO Sort this out
    (defun org-sort-list-by-checkbox-type-1 ()
      (if (looking-at org-list-full-item-re)
        (cdr (assoc (match-string 3)
               '(("[X]" . 1) ("[-]" . 2) ("[ ]" . 3) (nil . 4))))
        4))

    ;; Custom agendas
    ;; TODO Remove boilerplate
    ;; TODO Look at http://stackoverflow.com/questions/22394394/orgmode-a-report-of-tasks-that-are-done-within-the-week for how to tidy this up

    (defun sbw/make-title-string (title)
      (concat "\n" title "\n" (make-string (length title) ?-) "\n"))

    (setq org-agenda-prefix-format
      '( (agenda . " %i %-20:c%?-20t% s")
         (timeline . "  % s")
         (todo . " %i %-20:c")
         (tags . " %i %-20:c")
         (search . " %i %-20:c")) )

    (setq org-agenda-remove-tags 1)

    (setq org-agenda-custom-commands
      '( ("c" . "Custom agenda")
         ("cw" "Work agenda"
           ( (agenda ""
               ( (org-agenda-ndays 7)
                 (org-agenda-files sbw/org-work-files)
                 ))
                  
             (tags-todo "+PRIORITY=\"A\""
               ( (org-agenda-overriding-header (sbw/make-title-string "High priority tasks"))
                 (org-agenda-files sbw/org-work-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
                 (org-agenda-files sbw/org-work-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "+PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
                 (org-agenda-files sbw/org-work-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             )
           nil
           ("C:/Users/IBM_ADMIN/Dropbox/Private/org/work-agenda.html"))
     
         ("cp" "Personal agenda"
           ( (agenda ""
               ( (org-agenda-ndays 7)
                 (org-agenda-files sbw/org-personal-files)
                 ))
         
             (tags-todo "+PRIORITY=\"A\""
               ( (org-agenda-overriding-header (sbw/make-title-string "High priority tasks"))
                 (org-agenda-files sbw/org-personal-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "-PRIORITY=\"A\"&-PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Normal priority tasks"))
                 (org-agenda-files sbw/org-personal-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))

             (tags-todo "+PRIORITY=\"C\""
               ( (org-agenda-overriding-header (sbw/make-title-string  "Low priority tasks"))
                 (org-agenda-files sbw/org-personal-files)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))
                 ))
             )
           nil
           ("C:/Users/IBM_ADMIN/Dropbox/Private/org/personal-agenda.html"))
         ))

    ;; Appointments
    ;; Refresh when the agenda is displayed

    (defun sbw/org-refresh-appointments-from-agenda ()
      "Update the appointment list from the agenda."
      (interactive)
      (setq appt-time-msg-list nil)
      (org-agenda-to-appt))

    (add-hook 'org-finalize-agenda-hook 'sbw/org-refresh-appointments-from-agenda 'append)
    (appt-activate t)

    ;; org-protocol

    (require 'org-protocol)

    (setq org-protocol-default-template-key nil)

    (defun sbw/org-extract-rtc-description (s)
      (let* ( (regex "^\\(.* [0-9]+: \\)\\(.*\\)\\( - Change and Configuration Management\\)") )
        (if (string-match regex s)
          (match-string 2 s)
          s)))
    
    (defun sbw/org-capture-rtc-task ()
      ;; The docs state that placeholders will be expanded before evaluating Elisp, but that doesn't appear to be
      ;; happening in practice so we have to access the link via an internal variable.
      (let* ( (url   (plist-get org-store-link-plist :link))
              (descr (plist-get org-store-link-plist :description)) )
        (concat (sbw/org-extract-rtc-description descr) "\n" (s-lex-format "[[${url}][Link to RTC task]]"))))
    
    (setq org-capture-templates
      '( ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?%a\n%i")
         ("l" "Link" entry (file+olp org-default-notes-file "Links")
           "* TODO %?%a\n%i")
         ("r" "RTC task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?%(sbw/org-capture-rtc-task)")       
         ))

    ;; Sorting subtrees

    (defun sbw/org-multisort (&rest criteria)
      "Sort subtree by multiple criteria. See org-sort-entries for sort types."
      (interactive)
      (mapc #'(lambda (x) (org-sort-entries nil x))
        (reverse criteria)))

    (defun sbw/org-sort-subtree ()
      "Sort the current subtree by TODO state, priority, scheduled date, deadline, then alphabetic."
      (interactive)
      (if (org-clocking-p)
        (message "Currently clocked in on a task. Clock out and re-run the command to sort the subtree.")
        (save-excursion
          (sbw/org-multisort ?o ?p ?s ?d ?a)
          (hide-subtree)
          (org-cycle))))

    (defun sbw/sort-all-subtrees-in-buffer ()
      "Sorts all the subtrees in the current org-mode buffer."
      (interactive)
      (-each
        (->> (sbw/org-utils-heading-summaries-for-file (buffer-file-name))
          (-filter (lambda (x) (equal 1 (gethash :level x)))))
        (lambda (x)
          (goto-char (gethash :point x))
          (message (format "Sorting subtree under [%s]" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (ignore-errors (sbw/org-sort-subtree)))))

    ;; Reformat the current org-mode buffer

    (defun sbw/org-mode-reformat ()
      "Reformat the current org-mode buffer, updating dynamic blocks, sorting subtrees."
      (interactive)
      (org-update-all-dblocks)
      (sbw/sort-all-subtrees-in-buffer)
      nil)

    ;; Creating a new org file

    (defun sbw/org-new-org-file (project category)
      (interactive "sProject: \nsCategory: ")
      (let* ( (content (f-read-text (s-lex-format "${sbw/lisp-path}/sbw-org-review-new-file-template.org")))
              (path    (s-lex-format "${org-directory}/current/${project}/${category}.org" )))
        (f-write
          (->> content
            (s-replace-all `(("${category}" . ,category))))
          'utf-8
          path)
        (sbw/org-find-org-files)
        (message "Created and added %s" path)))

    ;; Stuff to rationalise

    (setq org-fontify-done-headline t)
    (custom-set-faces
      '(org-done ((t (                  ;:foreground "PaleGreen"   
                       :weight normal
                       :strike-through t))))
      '(org-headline-done 
         ((((class color) (min-colors 16) (background dark)) 
            (                           ; :foreground "LightSalmon"
              :strike-through t))))
      )


    (setq org-clock-heading-function
      (lambda () (sbw/truncate-string (nth 4 (org-heading-components)) 30))))

  :bind
  ("C-c o s p" . org-priority)
  ("C-c o s e" . org-set-effort)
  ("C-c o s s" . org-schedule)
  ("C-c o s d" . org-deadline)
  ("C-c o s t" . org-todo)
  ("C-c o c i" . org-clock-in)
  ("C-c o c o" . org-clock-out)
  ("C-c o c g" . org-clock-goto)
  ("C-c o r r" . sbw/org-mode-reformat)
  ("C-c o r s" . sbw/org-sort-subtree)
  ("C-c o v n" . org-narrow-to-subtree)
  ("C-c o v w" . widen)
  ("C-c o c l" . org-insert-link)
  ("C-c o c c" . org-capture)
  ("C-c o f"   . fill-paragraph)
  ("C-c o F"   . sbw/unfill-paragraph))

;; TODO Move somewhere more sensible
(defun sbw/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line."
  (interactive)
  (let* ( (fill-column (point-max)) )
    (fill-paragraph nil)))

(defun sbw/unfill-region (start end)
  "Convert a multi-line region into a single line."
  (interactive "r")
  (let* ( (fill-column (point-max)) )
    (fill-region start end nil)))

(provide 'sbw-configure-org-mode)
