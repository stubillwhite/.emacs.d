;; Functions for finding files within my org-mode file structure, which is of the form:
;;
;;     project-one
;;       category-one
;;         foo.org
;;         bar.org
;;       category-two
;;         baz.org
;;     project-two
;;       category-three
;;         gonk.org

(require 's)
(require 'f)
(require 'dash)

(defun sbw/org-config--categorise-files (files)
  (-reduce-from
    (lambda (acc x)
      (let* ( (path    (s-split "/" (f-relative x org-directory)))
              (stack   (car path))
              (project (cadr path)) )
        (sbw/ht-update-in acc (vector stack project) (lambda (y) (cons x y)))))
    (sbw/ht-create)
    files))

(defun sbw/org-config ()
  "Returns information about my org-mode configuration."
  (let* ( (org-file? (lambda (x) (s-ends-with? ".org" (f-filename x))))
          (all-files (f-entries org-directory org-file? :recursive)) )
    (sbw/ht-create
      :all-files   all-files
      :categorised (sbw/org-config--categorise-files all-files))))

(defun sbw/org-config-projects (config)
  "Returns a list of org-mode projects."
  (sbw/ht-keys (sbw/ht-get config :categorised)))

(defun sbw/org-config-categories (config)
  "Returns a list of org-mode categories."
  (seq-uniq
    (seq-mapcat
      'sbw/ht-keys
      (sbw/ht-vals (sbw/ht-get config :categorised))())))

(defun sbw/org-config-files (config projects categories)
  "Returns the org-mode files filtered by the specified PROJECTS
and CATEGORIES, where PROJECTS and CATEGORIES are lists of string
names, or nil to indicate that all should be included."
  (let* ( (projects    (or projects   (sbw/org-config-projects config)))
          (categories  (or categories (sbw/org-config-categories config)))
          (filter-vals (lambda (ht ks) (sbw/ht-vals (sbw/ht-select-keys ht ks)))) )
    (apply (-partial 'seq-concatenate 'list)
      (seq-mapcat
        (lambda (x) (funcall filter-vals x categories))
        (funcall filter-vals (sbw/ht-get config :categorised) projects)))))

(provide 'sbw-org-config)
