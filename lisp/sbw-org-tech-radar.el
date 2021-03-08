(require 'org)
(require 'sbw-org-utils)
(require 's)

(defun sbw/org-tech-radar--to-tsv (org-heading)
  (-let (((&hash :category category :state state :heading heading) org-heading)
         (is-new                                                   "false")
         (description                                              "tbd"))
    (s-join "\t" (list heading category state is-new description))))

(defun sbw/org-tech-radar-generate-tech-radar (filename)
  (->> (sbw/org-utils-heading-summaries-for-file filename)
       (-filter (lambda (x) (sbw/ht-get x :state)))
       (-map 'sbw/org-tech-radar--to-tsv)
       (s-join "\n")
       (message)))

(defun sbw/org-tech-radar--write-file (filename content)
  (let* ((revert-without-query (list filename)))
    (f-mkdir (f-dirname filename))
    (with-temp-file filename (insert content))
    (message (format "Created '%s'" filename))
    (find-file filename)
    nil))

(provide 'sbw-org-tech-radar)


;; (->> "~/Dropbox/Private/org/current/work/tech-radar.org"
;;      (sbw/org-tech-radar-generate-tech-radar)
;;      )


