(defun sbw/value-eq--data-type (x &rest args)
  (cond
    ((eq nil x)       :nil)
    ((stringp x)      :string)
    ((keywordp x)     :keyword)
    ((symbolp x)      :symbol)
    ((numberp x)      :number)
    ((hash-table-p x) :hash-table)
    ((listp x)        :list)
    ((vectorp x)      :vector)
    (t                (signal 'wrong-type-argument (list x)))))

(defun sbw/value-eq--lists-equal-ignoring-order (a b)
  (and
    (eq (length a) (length b))
    (not
      (-reduce-from
        (lambda (acc v) (-filter (lambda (x) (not (sbw/value-eq x v))) acc))
        a
        b))))

(defun sbw/value-eq--hash-tables-equal (a b)
  (if (sbw/value-eq--lists-equal-ignoring-order (sbw/ht2-keys a) (sbw/ht2-keys b))
    (-reduce-from
      (lambda (acc k) (and acc (sbw/value-eq (sbw/ht2-get a k) (sbw/ht2-get b k))))
      t
      (sbw/ht-keys a))))

(defun sbw/value-eq--lists-equal (a b)
  (-reduce-from
    (lambda (acc v) (and acc (sbw/value-eq (car v) (cdr v))))
    t
    (-zip-fill :sbw/value-eq--padding a b)))

(defun sbw/value-eq--vectors-equal (a b)
  (-reduce-from
    (lambda (acc v) (and acc (sbw/value-eq (car v) (cdr v))))
    t
    (-zip-fill :sbw/value-eq--padding a b)))

(defun sbw/value-eq (a b)
  "Return t if A and B are equal, nil otherwise."
  (let* ( (dt-a (sbw/value-eq--data-type a))
          (dt-b (sbw/value-eq--data-type b)) )
    (when (eq dt-a dt-b)
      (cond
        ((eq dt-a :nil)        (eq a b))
        ((eq dt-a :string)     (string-equal a b))
        ((eq dt-a :keyword)    (eq a b))
        ((eq dt-a :number)     (= a b))
        ((eq dt-a :hash-table) (sbw/value-eq--hash-tables-equal a b))
        ((eq dt-a :list)       (sbw/value-eq--lists-equal a b))
        ((eq dt-a :vector)     (sbw/value-eq--vectors-equal a b))))))

(defun sbw/value-eq--stringify-hash-table (a)
  (let* ( (stringify-k-and-v (lambda (acc v) (cons (concat (sbw/value-eq--stringify v) (sbw/value-eq--stringify (gethash v a))) acc)))
          (key-value-pairs   (-reduce-from stringify-k-and-v '() (hash-table-keys a))) )
    (concat
      "{" 
      (-reduce-from 
        (lambda (acc v) (concat acc v))
        ""
        (sort key-value-pairs 'string<))
      "}")))

(defun sbw/value-eq--stringify-list (a)
  (concat
    "("
    (-reduce-from
      (lambda (acc v) (concat acc (sbw/value-eq--stringify v)))
      ""
      a)
    ")"))

(defun sbw/value-eq--stringify-vector (a)
  (let* ( (idx 0)
          (s   "[") )
    (while (< idx (length a))
      (setq s (concat s (sbw/value-eq--stringify (elt a idx))))
      (setq idx (sbw/inc idx)))
    (concat s "]")))

(defun sbw/value-eq--stringify (x)
  (let* ( (dt-x (sbw/value-eq--data-type x)) )
    (cond
      ((eq dt-x  :nil)        (format "%s" x))
      ((eq dt-x  :string)     (format "%s" x))
      ((eq dt-x  :keyword)    (format "%s" x))
      ((eq dt-x  :number)     (format "%s" x))
      ((eq dt-x  :hash-table) (sbw/value-eq--stringify-hash-table x))
      ((eq dt-x  :list)       (sbw/value-eq--stringify-list x))
      ((eq dt-x  :vector)     (sbw/value-eq--stringify-vector x)))))

(defun sbw/value-eq-hashcode (x)
  (sxhash (sbw/value-eq--stringify x)))

(define-hash-table-test 'sbw/value-eq-test 'sbw/value-eq 'sbw/value-eq-hashcode)

(provide 'sbw-value-eq)

