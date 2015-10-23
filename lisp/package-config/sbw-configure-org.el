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

    ;; General settings

    (setq 
      org-list-empty-line-terminates-plain-lists t                            ;; Single empty line terminates plain lists
      org-blank-before-new-entry                 '( (heading         . nil)   ;; No blank lines before headings
                                                    (plain-list-item . nil) ) ;; No blank lines before plain list items
      org-clock-into-drawer                      t                            ;; Clock into drawers
      org-src-fontify-natively                   t                            ;; Fontify embedded code blocks
      org-ellipsis                               "\u2026"                     ;; Small ellipsis character
      org-agenda-fontify-priorities              nil                          ;; Don't let priority change task representation
      org-indent-mode                            t                            ;; Use indent mode
      org-log-into-drawer                        t                            ;; Log into drawers
      org-log-done                               'time                        ;; Timestamp task completion so it can be used in reports
      org-M-RET-may-split-line                   nil                          ;; Don't split lines
      org-return-follows-link                    t                            ;; Easy link navigation
      org-use-property-inheritance               t                            ;; Child items should inherit all from parents
      org-default-priority                       ?B                           ;; Default priority for unprioritised items
      appt-display-interval                      5                            ;; Reminder for an appointment every five minutes...
      appt-message-warning-time                  15                           ;; ...starting fifteeen minutes before it is due
      org-frame-title-format-backup              sbw/frame-title-format       ;; Override title frame title format
      org-tag-alist                              nil                          ;; No tags
      org-startup-folded                         'content                     ;; Display content when first opening org files
      )

    ;; Display elapsed time as hours and minutes only
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    
    ;; Start with source blocks hidden
    (add-hook 'org-mode-hook 'org-hide-block-all)

    ;; Open org-mode links in the same frame
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
    
    ;; Keywords and preferred sort order
    (setq
     org-todo-keywords       '("TODO(t)" "STARTED(s)" "BLOCKED(b)" "POSTPONED(p)" "|" "DONE(d!)" "CANCELLED(c)")
     sbw/org-todo-sort-order '("STARTED" "BLOCKED" "TODO" "POSTPONED" "DONE" "CANCELLED"))

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
           "* TODO Check out %?%a\n%i")
         ("r" "RTC task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?%(sbw/org-capture-rtc-task)") ))

    ;; Sorting subtrees

    (defun sbw/org-multisort (&rest criteria)
      "Sort subtree by multiple criteria. See org-sort-entries for sort types."
      (interactive)
      (mapc #'(lambda (x) (org-sort-entries nil x))
        (reverse criteria)))

    (defun sbw/org-sort-subtree ()
      "Sort the current subtree by TODO state, priority,
scheduled date, deadline, then alphabetic."
      (interactive)
      (if (org-clocking-p)
        (message "Currently clocked in on a task. Clock out and re-run the command to sort the subtree.")
        (let* ( (original-value org-todo-keywords-1) )
          (setq org-todo-keywords-1 sbw/org-todo-sort-order)
          (save-excursion
            (sbw/org-multisort ?o ?p ?s ?d ?a)
            (hide-subtree)
            (org-cycle))
          (setq org-todo-keywords-1 original-value))))

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

    ;; Stuff to rationalise

    (setq org-fontify-done-headline t)
    
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

