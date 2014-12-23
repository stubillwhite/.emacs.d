(require 'sbw-org-review)


(ert-deftest sbw/org-review-then-generates-expected-report ()
  (let* ( (org-file-dir "~/.emacs.d/lisp/test/org-review-test")
          (temp-dir     (concat org-file-dir "/temp/org-review-test"))
          (org-files    (-map (lambda (x) (concat org-file-dir x)) (list "/test-file-one.org" "/test-file-two.org" "/test-file-three.org")))
          (report-fnam  (concat temp-dir "/actual-report.txt"))
          (start        (sbw/compose-time
                          (sbw/ht-create
                            :second          0
                            :minute          0
                            :hour            0
                            :day             1
                            :month           1
                            :year            1970
                            :weekday         4
                            :daylight-saving nil
                            :timezone        0
                            )))
          (end          (sbw/compose-time
                          (sbw/ht-create
                            :second          0
                            :minute          0
                            :hour            0
                            :day             15
                            :month           1
                            :year            1970
                            :weekday         4
                            :daylight-saving nil
                            :timezone        0
                            )))

          (test-config  (-> (sbw/org-review-config-previous-week org-files)
                          (sbw/ht-assoc :filename report-fnam)
                          (sbw/ht-assoc :start    start)
                          (sbw/ht-assoc :end      end))) )
    
    (mkdir temp-dir :create-parents)
    (sbw/org-review test-config)

    (should (string=
              (f-read-text report-fnam)
              (f-read-text (concat org-file-dir "/expected-report.txt"))))

                                        
    ))




;; TODO Replace with f-read-text
;(defun file-content (filename)
;  "Returns the content of filename as a string."
;  (with-temp-buffer
;    (insert-file-contents filename)
;    (buffer-string)))

;(defun sbw/-org-review-test-org-files ()
;  (-map
;    (lambda (x) (concat "~/.emacs.d/lisp/test/org-review" x))
;    (list "/test-file-one.org" "/test-file-two.org")))

;(ert-deftest sbw/org-review-generate-category-report-then-expected-report ()
;  (should (string=
;            (sbw/org-review-generate-category-report (sbw/-org-review-test-org-files) 'sbw/-org-review-default-formatter-func)
;            (file-content "expected-report.txt"))))




(message "%s" (sbw/decompose-time (current-time)))
(sbw/compose-time
  (sbw/ht-create
    :second          0
    :minute          0
    :hour            0
    :day             1
    :month           1
    :year            1970
    :weekday         4
    :daylight-saving nil
    :timezone        0
    ))

(message "%s" (sbw/decompose-time '(0 0 0 0)))


(provide 'sbw-org-reports-test)
