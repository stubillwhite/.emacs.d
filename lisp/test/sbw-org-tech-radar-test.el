(require 'sbw-org-tech-radar)

(lexical-let* ((test-file (lambda (x) (format "%s/test/org-tech-radar-test/%s" sbw/lisp-path x)))
               (file-text (lambda (x) (f-read-text (funcall test-file x)))))

  (ert-deftest sbw/org-tech-radar-generate-tech-radar-then-expected-tech-radar ()
    "sbw/org-review-completed-tasks-generate-report then expected report"
    (lexical-let* ((filename  (funcall test-file "tech-radar.org"))
                   (expected  (funcall file-text "tech-radar-expected.txt"))
                   (actual    (sbw/org-tech-radar-generate-tech-radar filename)) )
      (should (string= actual expected))))
)

(provide 'sbw-org-tech-radar-test)
