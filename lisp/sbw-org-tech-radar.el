;;; sbw/org-tech-radar.el --- Generating a technology radar from Org

(require 'org)
(require 'sbw-org-utils)
(require 's)

(defun sbw/org-tech-radar--extract-description (raw-text)
  (message raw-text)
  (->> raw-text
       (s-split "\n")
       (-drop-while (lambda (x) (not (s-match "#\\+begin_src markdown" x))))
       (-drop 1)
       (-take-while (lambda (x) (not (s-match "#\\+end_src" x))))
       (-map 's-trim)
       (s-join " ")))

(defun sbw/org-tech-radar--to-tsv (org-heading)
  (-let* (((&hash :category category :state state :heading heading :raw-text raw-text) org-heading)
          (is-new      "false")
          (description (sbw/org-tech-radar--extract-description raw-text)))
    (s-join "\t" (list heading state category is-new description))))

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
       (-map 'sbw/org-tech-radar--to-tsv)
       (s-join "\n")
       (s-append "\n")
       (message)))

;; TODO: Move to utils
(defun sbw/org-tech-radar--write-file (filename content)
  (let* ((revert-without-query (list filename)))
    (f-mkdir (f-dirname filename))
    (with-temp-file filename (insert content))
    (message (format "Created '%s'" filename))
    (find-file filename)
    nil))

(provide 'sbw-org-tech-radar)

(defun white-test ()
  (interactive)
  (->> "~/Dropbox/Private/org/current/work/tech-radar.org"
       (sbw/org-tech-radar-generate-tech-radar)
       (sbw/org-tech-radar--write-file "~/trash/test.tsv")
       ))


