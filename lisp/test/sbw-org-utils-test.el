(require 'sbw-org-utils)

(lexical-let* ( (start    (days-to-time 0))
                (end      (days-to-time 7))
                (filename "/Users/white1/.emacs.d/lisp/test/org-utils-test/input.org")
                (expected-summary-1 (sbw/ht-create :filename filename
                                                   :point    1
                                                   :category nil
                                                   :state    nil
                                                   :tags     nil
                                                   :heading  "Title"
                                                   :level    1
                                                   :clock    nil
                                                   :closed   nil))
                (expected-summary-2 (sbw/ht-create :filename filename
                                                   :point    9
                                                   :category nil
                                                   :state    "TODO"
                                                   :tags     nil
                                                   :heading  "Task one"
                                                   :level    2
                                                   :clock    (list
                                                              "[2016-09-24 Sat 13:54]--[2016-09-25 Sun 13:54]"
                                                              "[2016-09-23 Fri 13:54]--[2016-09-24 Sat 07:54]")
                                                   :closed   nil)) )

  (ert-deftest sbw/org-utils-heading-summaries-for-file-then-expected-summaries ()
    "sbw/org-utils-heading-summaries-for-file then expected summaries"
    (lexical-let* ( (actual   (sbw/org-utils-heading-summaries-for-file filename))
                    (expected (list expected-summary-1
                                    expected-summary-2)))
      (should (sbw/value-eq actual expected))))

  ;; (pp (sbw/org-utils-heading-summaries-for-file filename)) 
  ;; (sbw/pprint-as-json (sbw/org-utils-heading-summaries-for-file filename))
  nil)



(provide 'sbw-org-utils-test)
