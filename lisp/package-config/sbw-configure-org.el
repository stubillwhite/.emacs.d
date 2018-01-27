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
           ((sbw/is-linux?)   "~/Dropbox/Private/org")
           ((sbw/is-windows?) "/cygdrive/c/Users/IBM_ADMIN/Dropbox/Private/org")
           ((sbw/is-darwin?)  "~/Dropbox/Private/org")))

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
     org-emphasis-alist                         ()                           ;; Don't be fancy
     org-agenda-fontify-priorities              nil                          ;; Don't let priority change task representation
     org-duration-format                        'h:mm                        ;; Display durations in hours, not days
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
     org-hide-block-startup                     t                            ;; Do not display code block content when opening org files
     org-context-in-file-links                  nil                          ;; Don't store position when creating file links
     org-src-window-setup                       'current-window              ;; Edit source blocks in the current frame
     org-image-actual-width                     '(40)                        ;; Default 100px, unless there is a #+ATTR.*: width="200"
     org-use-speed-commands                     t                            ;; Enable speed commands
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
     org-babel-sh-command        "zsh -i" ;; Interactive Zsh for shell
     org-confirm-babel-evaluate  nil      ;; Don't ask confirmation to execute
     )

    (require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider)

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

    (defun sbw/org-extract-jira-description (s)
      (setq white-debug s)
      (let* ( (regex "^\\(.*\\) - \\(JIRA\\|Mendeley JIRA\\)") )
        (if (string-match regex s)
            (match-string 1 s)
          s)))
    
    (defun sbw/org-capture-jira-task ()
      ;; The docs state that placeholders will be expanded before evaluating Elisp, but that doesn't appear to be
      ;; happening in practice so we have to access the link via an internal variable.
      (let* ( (url   (plist-get org-store-link-plist :link))
              (descr (plist-get org-store-link-plist :description)) )
        (concat (sbw/org-extract-jira-description descr) "\n" (s-lex-format "[[${url}][Link to task]]\n"))))
    
    (setq org-capture-templates
          '( ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
              "* TODO [#C] Check out %?%a\n%i")
             ("j" "JIRA task" entry (file+headline org-default-notes-file "Tasks")
              "* TODO %?%(sbw/org-capture-jira-task)") ))

    ;; Bookmarklet
    ;; javascript:location.href='org-protocol://store-markdown-link://'+encodeURIComponent(location.href)+'//'+encodeURIComponent(document.title)

    (defun sbw/org-protocol-store-markdown-link (arg-str) 
      (let* ( (args          (org-protocol-split-data arg-str t org-protocol-data-separator))
              (uri           (org-protocol-sanitize-uri (car args)))
              (title         (cadr args))
              (markdown-link (s-lex-format "[${title}](${uri})")) )
        (kill-new markdown-link)
        (message "`%s' to insert `%s'" (substitute-command-keys"\\[yank]") uri))
      nil)

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

    ;; When storing file links, abbreviate the description to just the filename if not set

    (defun sbw/org-make-link-description (link description)
      (if (and (s-starts-with? "file" link) (not description))
          (f-filename (f-expand (nth 1 (s-split-up-to ":" link 1))))
        description))
    (setq org-make-link-description-function 'sbw/org-make-link-description)
    
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
  ("C-c o F"   . sbw/unfill-paragraph)
  ("C-c o a"   . sbw/org-config-archive-task))

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

;; (setq org-priority-faces '((?A . (:weight 'bold))
;;                            (?B . (:weight 'bold))
;;                            (?C . (:italic t)))
;;       org-agenda-fontify-priorities t)

(provide 'sbw-configure-org-mode)

