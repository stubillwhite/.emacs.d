(require 'sbw-hash-tables)
(require 'dash)
(require 'names)

(define-namespace sbw/time-

  (defun decompose (time)
    "Returns TIME in the form of a hash-table of fields, the keys of which are 
    :second :minute :hour :day :month :year :weekday :daylight-saving :timezone"
    (apply 'sbw/ht-create
      (apply '-concat
        (-zip-with
          'list
          (list :second :minute :hour :day :month :year :weekday :daylight-saving :timezone)
          (decode-time time)))))

  (defun compose (decomposed)
    "Returns the time represented by DECOMPOSED."
    (apply 'encode-time
      (-reduce-from
        (lambda (acc x) (cons (gethash x decomposed) acc))
        '()
        (list :timezone :daylight-saving :weekday :year :month :day :hour :minute :second))))

  (defun -retain-fields (time fields-to-retain)
    (-let* ( (epoch-time (seconds-to-time 0)) ) 
      (compose
        (sbw/ht-merge
          (decompose epoch-time)
          (sbw/ht-select-keys (decompose time) fields-to-retain)))))
  
  (defun as-date-only (time)
    "Returns TIME with just the date set and the time zeroed out."
    (-retain-fields time (list :day :month :year :weekday :daylight-saving :timezone)))

  (defun as-time-only (time)
    "Returns TIME with just the time set and the date zeroed out."
    (-retain-fields time (list :second :minute :hour :timezone)))

  (defun adjust-by (time days)
    "Returns TIME adjusted by DAYS."
    (days-to-time (+ (time-to-number-of-days date) n)))

  (defun max (t1 t2)
    "Returns the maximum of T1 and T2."
    (if (time-less-p t1 t2) t2 t1))

  (defun min (t1 t2)
    "Returns the minimum of T1 and T2"
    (if (time-less-p t1 t2) t1 t2))
  
  )

(provide 'sbw-time)
