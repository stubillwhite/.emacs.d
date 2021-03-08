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
    (s-join "\t" (list heading category state is-new description))))

(sbw/org-tech-radar--extract-description "#+begin_src markdown\nfoo bar baz\n#+end_src")
(s-split "\n" "#+begin_src markdown\nfoo bar baz\n#+end_src")


(defun sbw/org-tech-radar-generate-tech-radar (filename)
  (->> (sbw/org-utils-heading-summaries-for-file filename)
       (-filter (lambda (x) (sbw/ht-get x :state)))
       (-map 'sbw/org-tech-radar--to-tsv)
       (s-join "\n")
       (message)
       ))

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


