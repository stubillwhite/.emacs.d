;;; sbw/org-tech-radar.el --- Generating a technology radar from Org

(require 'magit)
(require 'markdown)
(require 'org)
(require 'sbw-org-utils)
(require 's)

(defun sbw/org-tech-radar--to-markdown (text)
  (let* ((input-buffer     "tmp.md")
         (output-buffer    "markdown-fragment")
         (markdown-command (concat "pandoc --from=gfm --to=html --standalone")))
    (with-temp-buffer input-buffer
      (insert text)                
      (markdown output-buffer)
      (with-current-buffer output-buffer
        (->> (buffer-string)
             (s-split "\n")
             (-drop-while (lambda (x) (not (s-match "<body>" x))))
             (-drop 1)
             (-take-while (lambda (x) (not (s-match "</body>" x))))
             (-map 's-trim)
             (s-join " "))))))

(defun sbw/org-tech-radar--extract-description (raw-text)
  (->> raw-text
       (s-split "\n")
       (-drop-while (lambda (x) (not (s-match "#\\+begin_src markdown" x))))
       (-drop 1)
       (-take-while (lambda (x) (not (s-match "#\\+end_src" x))))
       (s-join "\n")
       (sbw/org-tech-radar--to-markdown)))

(defun sbw/org-tech-radar--to-csv (org-heading)
  (-let* (((&hash :category category :state state :heading heading :raw-text raw-text) org-heading)
          (is-new      "false")
          (description (sbw/org-tech-radar--extract-description raw-text)))
    (->> (list heading state category is-new description)
         (-map (lambda (s) (s-replace-all `(("\"" . "\"\"")) s)))
         (s-join "\",\"")
         (s-append "\"")
         (s-prepend "\""))))

(defun sbw/org-tech-radar--summary-comparator (a b)
  (-let* (((&hash :category a-category :state a-state :heading a-heading) a)
          ((&hash :category b-category :state b-state :heading b-heading) b)
          (a-state-idx (-elem-index a-state org-todo-keywords-1))
          (b-state-idx (-elem-index b-state org-todo-keywords-1)))
    (cond
     ((< a-state-idx b-state-idx) t)
     ((and (= a-state-idx b-state-idx) (string< a-category b-category)) t)
     ((and (= a-state-idx b-state-idx) (string= a-category b-category) (string< a-heading b-heading)) t))))

(defun sbw/org-tech-radar-generate-tech-radar (filename)
  (->> (sbw/org-utils-heading-summaries-for-file filename)
       (-filter (lambda (x) (sbw/ht-get x :state)))
       (-sort 'sbw/org-tech-radar--summary-comparator)
       (-map 'sbw/org-tech-radar--to-csv)
       (s-join "\n")
       (s-append "\n")
       (message)))

(defun sbw/org-tech-radar-generate ()
  (interactive)
  (message "Generating tech radar")
  (->> "~/Dropbox/Private/org/current/work/tech-radar.org"
       (sbw/org-tech-radar-generate-tech-radar)
       (sbw/org-tech-radar--write-file "~/Dev/my-stuff/tech-radar/tech-radar.csv")))

(defun sbw/org-tech-radar-commit ()
  (interactive)
  (when (magit-anything-modified-p)
    (message "Tech radar has changed; committing and pushing changes")
    (magit-run-git "commit" "-am" "Update tech radar")
    (magit-run-git "push")))

(defun sbw/org-tech-radar-open ()
  (interactive)
  (message "Opening tech radar")
  (shell-command "open 'https://radar.thoughtworks.com/?sheetId=https%3A%2F%2Fraw.githubusercontent.com%2Fstubillwhite%2Ftech-radar%2Fmain%2Ftech-radar.csv'"))

(defun sbw/org-tech-radar-regenerate-and-open ()
  (interactive)
  (sbw/org-tech-radar-generate)
  (sbw/org-tech-radar-commit)
  (sbw/org-tech-radar-open))

;; TODO: Move to utils
(defun sbw/org-tech-radar--write-file (filename content)
  (let* ((revert-without-query (list filename)))
    (f-mkdir (f-dirname filename))
    (with-temp-file filename
      (insert (s-join "," '("name" "ring" "quadrant" "isNew" "description")))
      (insert "\n")
      (insert content))
    (message (format "Created '%s'" filename))
    (find-file filename)
    nil))

(provide 'sbw-org-tech-radar)
