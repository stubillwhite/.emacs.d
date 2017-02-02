(require 'use-package)

(use-package org-gcal
  :init
  (progn

    (setq org-gcal-file-alist (list (cons "stubillwhite@gmail.com"                               (f-join org-directory "current" "calendar" "stubillwhite.org"))
                                    (cons "3pnviif0ku4v051fmi9eodjvh0@group.calendar.google.com" (f-join org-directory "current" "calendar" "stu-and-nat.org"))
                                    (cons "3g5cjltp58i7eh77vpvnvpb8g4@group.calendar.google.com" (f-join org-directory "current" "calendar" "natalia.org"))))
     
    (defun sbw/org-gcal-synchronise-calendar ()
      "Synchronise the local calendar with Google Calenar"
      (interactive)
      (sbw/load-secrets)
      (org-gcal-refresh-token)
      (org-gcal-fetch))))

(provide 'sbw-configure-org-gcal)
