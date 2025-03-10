(use-package org
  :init
  (progn
    (require 'org-clock)
    (require 'sbw-org-utils)
    
    ;; Default to clean view with no leading asterisks for indentation
    (setq-default org-startup-indented t)

    ;; Org files

    (setq org-directory (sbw/dropbox-subfolder "Private/org"))

    (defun sbw/org-files (&rest dirs)
      "Return a list of the org files in directories DIRS."
      (-mapcat
       (lambda (x) (directory-files x :absolute org-agenda-file-regexp))
       (apply 'list dirs)))

    ;; General settings

    (setq 
     org-list-empty-line-terminates-plain-lists    t                            ;; Single empty line terminates plain lists
     org-blank-before-new-entry                    '( (heading         . nil)   ;; No blank lines before headings
                                                      (plain-list-item . nil) ) ;; No blank lines before plain list items
     org-clock-into-drawer                         t                            ;; Clock into drawers
     org-src-fontify-natively                      t                            ;; Fontify embedded code blocks
     org-ellipsis                                  "\u2026"                     ;; Small ellipsis character
     org-emphasis-alist                            ()                           ;; Don't be fancy
     org-agenda-fontify-priorities                 nil                          ;; Don't let priority change task representation
     org-duration-format                           'h:mm                        ;; Display durations in hours, not days
     org-indent-mode                               t                            ;; Use indent mode
     org-log-into-drawer                           t                            ;; Log into drawers
     org-log-done                                  'time                        ;; Timestamp task completion so it can be used in reports
     org-M-RET-may-split-line                      nil                          ;; Don't split lines
     org-return-follows-link                       t                            ;; Easy link navigation
     org-use-property-inheritance                  t                            ;; Child items should inherit all from parents
     org-default-priority                          ?B                           ;; Default priority for unprioritised items
     appt-display-interval                         5                            ;; Reminder for an appointment every five minutes...
     appt-message-warning-time                     15                           ;; ...starting fifteeen minutes before it is due
     org-frame-title-format-backup                 sbw/frame-title-format       ;; Override title frame title format
     org-tag-alist                                 nil                          ;; No tags
     org-startup-folded                            'content                     ;; Display content when first opening org files
     org-hide-block-startup                        t                            ;; Do not display code block content when opening org files
     org-context-in-file-links                     nil                          ;; Don't store position when creating file links
     org-src-window-setup                          'current-window              ;; Edit source blocks in the current frame
     org-image-actual-width                        '(40)                        ;; Default 100px, unless there is a #+ATTR.*: width="200"
     org-use-speed-commands                        t                            ;; Enable speed commands
     org-export-with-latex                         'imagemagick                 ;; Export LaTeX snippets
     )

    ;; Babel
    
    (org-babel-do-load-languages                'org-babel-load-languages
                                                '((clojure    . t)
                                                  (dot        . t)
                                                  (emacs-lisp . t)
                                                  (perl       . t)
                                                  (python     . t)
                                                  (ruby       . t)
                                                  (shell      . t)))

    (setq org-babel-default-header-args         '((:results . "replace value drawer")))
    
    (setq
     ;; org-babel-sh-command        "zsh -i" ;; Interactive Zsh for shell
     org-confirm-babel-evaluate  nil      ;; Don't ask confirmation to execute
     )

    (defun sbw/org-babel-handle-ansi-codes-in-output ()
      (interactive)
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (src)
          (when (equalp "sh" (org-element-property :language src))
            (let ((begin (org-element-property :begin src))
                  (end   (org-element-property :end src)))
              (ansi-color-apply-on-region begin end))))))

    (add-to-list 'org-babel-after-execute-hook #'sbw/org-babel-handle-ansi-codes-in-output)
    
    (require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider)

    (require 'orgtbl-aggregate)
    
    (setq org-babel-default-header-args:clojure
          (cons '(:results . "replace output drawer")
                (assq-delete-all :results org-babel-default-header-args)))

    (defun sbw/org--construct-edit-buffer-name-advice (orig-fun &rest args)
      "Construct a prettier buffer name for a source editing buffer."
      (let* ( (org-buffer-name (car args))
              (lang            (cadr args)) )
        (concat org-buffer-name "-src-" lang)))

    (advice-add 'org-src--construct-edit-buffer-name :around #'sbw/org--construct-edit-buffer-name-advice)
    
    ;; Capture standard error when executing shell blocks
    ;; (setq org-babel-default-header-args:sh '((:prologue . "exec 2>&1")
    ;;                                          (:epilogue . ":")))
    
    ;; Display elapsed time as hours and minutes only
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    
    ;; Start with source blocks hidden
    (add-hook 'org-mode-hook 'org-hide-block-all)

    ;; Latex
    (setq org-latex-create-formula-image-program 'imagemagick                                     ;; Imagemagick for rendering
          exec-path                              (append '("/Library/TeX/texbin") exec-path)      ;; Add executable to the path
          org-format-latex-options               (plist-put org-format-latex-options :scale 2.0)  ;; Double size when rendering formulae
          )
                                                            
    ;; Open org-mode links in the same frame
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
    
    ;; Keywords and preferred sort order
    (setq
     sbw/org-todo-keywords   '("TODO(t)" "STARTED(s)" "BLOCKED(b)" "POSTPONED(p)" "|" "DONE(d!)" "CANCELLED(c)")
     sbw/org-todo-sort-order '("STARTED" "BLOCKED" "TODO" "POSTPONED" "DONE" "CANCELLED")
     org-todo-keywords       sbw/org-todo-keywords)

    (setq org-drawers
          '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES"))

    (setq org-archive-save-context-info
          '(time file ltags itags todo category olpath))

    ;; New headings should include created property and markdown content

    (defun sbw/org-heading-set-created-property ()
      (save-excursion
        (org-back-to-heading)
        (org-set-property "CREATED" (format-time-string "%Y-%m-%d %T"))))

    (defun sbw/org-heading-set-content ()
      (save-excursion
        (org-back-to-heading)
        (goto-char (org-element-property :end (org-element-at-point)))
        (insert "\n#+begin_src markdown\n#+end_src\n\n")))

    (defun sbw/org-heading-set-defaults ()
      (sbw/org-heading-set-created-property)
      (sbw/org-heading-set-content))

    (add-hook 'org-insert-heading-hook #'sbw/org-heading-set-defaults)
    
    ;; Tags

    (defun sbw/org-rename-tag (old new)
      (interactive "scurrent tag: \nsnew name: ")
      (org-map-entries
       (lambda ()
         (when (member old (org-get-tags))
           (org-toggle-tag new 'on)
           (org-toggle-tag old 'off)))
       (format "+%s" old)
       'agenda))

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
    ;; TODO Sort this out
    (defun org-sort-list-by-checkbox-type-1 ()
      (if (looking-at org-list-full-item-re)
          (cdr (assoc (match-string 3)
                      '(("[X]" . 1) ("[-]" . 2) ("[ ]" . 3) (nil . 4))))
        4))

    ;; org-protocol

    (require 'org-protocol)

    (setq org-protocol-default-template-key nil)

    (defun sbw/org-extract-jira-description (s)
      (let* ( (regex "^\\(.*\\) - \\(Jira\\|JIRA\\|Mendeley JIRA\\|Elsevier Technology JIRA\\)") )
        (if (string-match regex s)
            (match-string 1 s)
          s)))
    
    (defun sbw/org-capture-jira-task ()
      ;; The docs state that placeholders will be expanded before evaluating Elisp, but that doesn't appear to be
      ;; happening in practice so we have to access the link via an internal variable.
      (let* ( (url   (org-link-unescape (plist-get org-store-link-plist :link)))
              (descr (org-link-unescape (plist-get org-store-link-plist :description))) )
        (concat (sbw/org-extract-jira-description descr) "\n" (s-lex-format "[[${url}][Link to task]]\n"))))

    (defun sbw/org-capture-todo-task ()
      ;; The docs state that placeholders will be expanded before evaluating Elisp, but that doesn't appear to be
      ;; happening in practice so we have to access the link via an internal variable.
      (let* ( (url   (org-link-unescape (plist-get org-store-link-plist :link)))
              (descr (org-link-unescape (plist-get org-store-link-plist :description))) )
        (concat "Check out " (s-lex-format "[[${url}][${descr}]]"))))
    
    (setq org-capture-templates
          '( ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
              "* TODO %?%(sbw/org-capture-todo-task)"
              :before-finalize sbw/org-heading-set-defaults)
             ("j" "JIRA task" entry (file+headline org-default-notes-file "Tasks")
              "* TODO %?%(sbw/org-capture-jira-task)"
              :before-finalize sbw/org-heading-set-defaults) ))

    ;; TODO: Adding created timestamp
    ;; https://emacs.stackexchange.com/questions/45270/in-org-mode-how-can-i-make-a-post-capture-hook-run-only-for-certain-capture-tem
    ;; 
    ;; (add-hook 'org-capture-before-finalize-hook
    ;;       (lambda ()
    ;;         (if org-note-abort
    ;;             (message "WHITE aborted")
    ;;           (org-todo "TODO"))))

    ;; Bookmarklet
    ;; javascript:location.href='org-protocol://store-markdown-link?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)

    (defun sbw/org-protocol-store-markdown-link (x)
      (let* ((url            (org-link-unescape (plist-get x :url)))
             (title          (sbw/org-extract-jira-description (org-link-unescape (plist-get x :title))))
             (stripped-title (s-replace-regexp "\[][\]" "" title))
             (escaped-title  (org-link-escape stripped-title))
             (markdown-link  (s-lex-format "[${escaped-title}](${url})")))
        (kill-new markdown-link)
        (message "`%s' to insert `%s'" (substitute-command-keys"\\[yank]") uri)
        nil))

    (setq org-protocol-protocol-alist
          '(("store-markdown-link"
             :protocol "store-markdown-link"
             :function sbw/org-protocol-store-markdown-link)))

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
        (let* ((is-tech-radar?      (s-equals? "tech-radar.org" (file-name-nondirectory (buffer-file-name)))) 
               (sort-order          (if (not is-tech-radar?) sbw/org-todo-sort-order org-todo-keywords-1))
               (original-value      org-todo-keywords-1)
               (org-todo-keywords-1 sort-order))
          (save-excursion
            (sbw/org-multisort ?o ?p ?s ?d ?a)
            (hide-subtree)
            (org-cycle)))))
    
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

    ;; When storing file links, abbreviate the description to just the filename if not set

    (defun sbw/org-make-link-description (link description)
      (if (and (s-starts-with? "file" link) (not description))
          (f-filename (f-expand (nth 1 (s-split-up-to ":" link 1))))
        description))
    (setq org-make-link-description-function 'sbw/org-make-link-description)

    ;; Setting priorities

    (defun sbw/org-agenda-apply-to-headline (f)
      "Apply F to the current headline in an agenda buffer."
      ;; Based on org-agenda-set-property
      (org-agenda-check-no-diary)
      (let* ((ret-val  nil)
             (hdmarker (or (org-get-at-bol 'org-hd-marker)
                           (org-agenda-error)))
             (buffer (marker-buffer hdmarker))
             (pos (marker-position hdmarker))
             (inhibit-read-only t)
             newhead)
        (org-with-remote-undo buffer
          (setq ret-val
                (with-current-buffer buffer
                  (widen)
                  (goto-char pos)
                  (org-show-context 'agenda)
                  (funcall f))))
        ret-val))

    (defun sbw/org-set-property (prop val)
      "Set a property for the current headline, regardless of whether agenda is active or not."  
      (interactive)
      (if (eq major-mode 'org-agenda-mode)
          (sbw/org-agenda-apply-to-headline (lambda () (org-set-property prop val)))
        (org-set-property prop val)))

    (defun sbw/org-get-property (prop)
      "Gets the value of a property from the current headling, regardless of whether agenda is active or not."
      (interactive)
      (if (eq major-mode 'org-agenda-mode)
          (sbw/org-agenda-apply-to-headline (lambda () (org-entry-get (point) prop)))
        (org-entry-get (point) prop)))

    (defun sbw/org-delete-property (prop)
      "Delete a property from the current headline, regardless of whether agenda is active or not."  
      (interactive)
      (if (eq major-mode 'org-agenda-mode)
          (sbw/org-agenda-apply-to-headline (lambda () (org-delete-property prop)))
        (org-delete-property prop)))

    (defun sbw/org-set-priority ()
      "Set the priority of the current headline, regardless of whether agenda is active or not."  
      (interactive)
      (if (eq major-mode 'org-agenda-mode)
          (sbw/org-agenda-apply-to-headline (lambda () (org-priority)))
        (org-priority)))
    
    ;; Stuff to rationalise
    
    (setq org-fontify-done-headline t)
    
    (setq org-clock-heading-function
          (lambda () (sbw/truncate-string (nth 4 (org-heading-components)) 30))))

  :bind
  ("C-c o s m" . hydra-org-set-matrix/body)
  ("C-c o s e" . org-set-effort)
  ("C-c o s s" . org-schedule)
  ("C-c o s d" . org-deadline)
  ("C-c o s p" . sbw/org-set-priority)
  ("C-c o s t" . org-todo)
  ("C-c o s :" . org-set-tags-command)
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
  ("C-c o F"   . sbw/unfill-paragraph)
  ("C-c o a"   . sbw/org-config-archive-task))

;; TODO Move somewhere more sensible

(defun sbw/org-babel--copy-errors-to-output-advice (orig-fun &rest args)
  (let* ((exit-code (apply orig-fun args)))
    (if (> exit-code 0)
        (progn
          (insert "-- ERROR --\n\n")
          (insert-buffer-substring (get-buffer-create " *Org-Babel Error*"))
          (goto-char (point-min))
          (while (re-search-forward "" nil t)
            (replace-match "\n"))
          0)
      exit-code)))

(defun sbw/org-babel-copy-errors-to-output (enable)
  (if enable
      (advice-add 'org-babel--shell-command-on-region :around #'sbw/org-babel--copy-errors-to-output-advice)
    (advice-remove 'org-babel--shell-command-on-region #'sbw/org-babel--copy-errors-to-output-advice)))

(sbw/org-babel-copy-errors-to-output nil)

(provide 'sbw-configure-org-mode)


(defun focus-test ()
  (progn
    (setq org-tags-column (- 0 (window-body-width)))
    (org-align-tags)))

(defun sbw/org--enable-tags-realignment ()
  (add-function :after after-focus-change-function #'focus-test)
  )

  ;; (add-function :after after-focus-change-function #'focus-test)

    (defun sbw/set-org-tags-column-based-on-window-size ()
      "Set org-tags-column to right-align based on window size. Assumes that org-ellipsis is a string."
      (setq org-tags-column (- (- (window-width) (length org-ellipsis)))))

    (defun sbw/right-align-tags ()
      "Right-align the all tags in the buffer."
      (interactive)
      (sbw/set-org-tags-column-based-on-window-size)
      (org-align-all-tags)
      (redisplay t))

(add-function :after after-focus-change-function #'sbw/right-align-tags)


(defun sbw/org-heading-display-content ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (print (org-element-property :contents-begin (org-element-at-point)))
    (print (org-element-property :contents-end (org-element-at-point)))))
