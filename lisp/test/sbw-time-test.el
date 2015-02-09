(require 'sbw-time)
(require 'sbw-hash-tables)
(require 'dash)

(lexical-let* ( (epoch-time       (days-to-time 0))
                (later-time       (days-to-time 7))
                (epoch-decomposed (sbw/ht-create :second 0 :minute 0 :hour 1 :day 1 :month 1 :year 1970 :weekday 4 :daylight-saving nil :timezone 3600))
                (later-decomposed (sbw/ht-create :second 0 :minute 0 :hour 1 :day 8 :month 1 :year 1970 :weekday 4 :daylight-saving nil :timezone 3600)) )

  ;; sbw/time-decompose

  (ert-deftest sbw/time-decompose-then-returns-decomposed-time ()
    "sbw/time-decompose then returns decomposed time."
    (should (sbw/ht-equal (sbw/time-decompose epoch-time) epoch-decomposed)))

  ;; sbw/time-compose

  (ert-deftest sbw/time-compose-then-returns-composed-time ()
    "sbw/time-compose then returns composed time."
    (should (equal (sbw/time-compose epoch-decomposed) epoch-time)))

  ;; sbw/time-as-date-only

  (ert-deftest sbw/time-as-date-only-needs-implementing ()
    :expected-result :failed
    "TODO description"
    (should (equal t nil)))

  ;; sbw/time-as-time-only

  (ert-deftest sbw/time-as-time-only-needs-implementing ()
    :expected-result :failed
    "TODO description"
    (should (equal t nil)))

  ;; sbw/time-adjust-by

  (ert-deftest sbw/time-adjust-by-needs-implementing ()
    :expected-result :failed
    "TODO description"
    (should (equal t nil)))

  ;; sbw/time-max

  (ert-deftest sbw/time-max-then-greatest-time ()
    "sbw/time-max then greatest time."
    (should (equal (sbw/time-max epoch-time later-time) later-time)))

  ;; sbw/time-min

  (ert-deftest sbw/time-min-then-least-time ()
    "sbw/time-min then least time."
    (should (equal (sbw/time-min epoch-time later-time) epoch-time)))

  ;; sbw/time-sum
  
  (ert-deftest sbw/time-sum-needs-implementing ()
    :expected-result :failed
    "TODO description"
    (should (equal t nil)))

  )
