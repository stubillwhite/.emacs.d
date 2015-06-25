(require 'sbw-multimethods)

(defun sbw/data-type (x &rest args)
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

(defmacro sbw/value-eq--defmethod (type args equality-test)
  `(sbw/mm-defmethod sbw/value-eq ,type ,args
     (and
       (eq (sbw/data-type (car args)) (sbw/data-type (cadr args)))
       ,equality-test)))

(defun sbw/value-eq--hash-tables-equal (a b)
  (-reduce-from 
    (lambda (acc v)
      (-let* ( (key-a (car v))
               (key-b (cdr v))
               (item-a (gethash key-a a))
               (item-b (gethash key-b b))) 
        (and acc (sbw/value-eq key-a key-b) (sbw/value-eq item-a item-b))))
    t
    (-zip-fill :sbw/value-eq--padding (hash-table-keys a) (hash-table-keys b))))

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

(sbw/mm-defmulti sbw/value-eq 'sbw/data-type)
(sbw/value-eq--defmethod [:nil]        (a b) (eq a b))
(sbw/value-eq--defmethod [:string]     (a b) (string-equal a b))
(sbw/value-eq--defmethod [:keyword]    (a b) (eq a b))
(sbw/value-eq--defmethod [:number]     (a b) (= a b))
(sbw/value-eq--defmethod [:hash-table] (a b) (sbw/value-eq--hash-tables-equal a b))
(sbw/value-eq--defmethod [:list]       (a b) (sbw/value-eq--lists-equal a b))
(sbw/value-eq--defmethod [:vector]     (a b) (sbw/value-eq--vectors-equal a b))

(defun sbw/value-eq--hashcode-hash-table (a)
  (-reduce-from 
    (lambda (acc v)
      (+ acc (sbw/value-eq-hashcode v) (sbw/value-eq-hashcode (gethash v a))))
    (sxhash :hash-table)
    (hash-table-keys a)))

(defun sbw/value-eq--hashcode-list (a)
  (-reduce-from
    (lambda (acc v) (+ acc (sbw/value-eq-hashcode v)))
    (sxhash :list)
    a))

(defun sbw/value-eq--hashcode-vector (a)
  (let* ( (idx      0)
          (hashcode (sxhash :vector)) )
    (while (< idx (length a))
      (setq hashcode (+ hashcode (elt a idx)))
      (setq idx (sbw/inc idx)))
    hashcode))

(sbw/mm-defmulti sbw/value-eq-hashcode 'sbw/data-type)
(sbw/mm-defmethod sbw/value-eq-hashcode [:nil]        (a) (sxhash a))
(sbw/mm-defmethod sbw/value-eq-hashcode [:string]     (a) (sxhash a))
(sbw/mm-defmethod sbw/value-eq-hashcode [:keyword]    (a) (sxhash a))
(sbw/mm-defmethod sbw/value-eq-hashcode [:number]     (a) (sxhash a))
(sbw/mm-defmethod sbw/value-eq-hashcode [:nil]        (a) (sxhash a))
(sbw/mm-defmethod sbw/value-eq-hashcode [:hash-table] (a) nil)
(sbw/mm-defmethod sbw/value-eq-hashcode [:list]       (a) (sbw/value-eq--hashcode-list a))
(sbw/mm-defmethod sbw/value-eq-hashcode [:vector]     (a) (sbw/value-eq--hashcode-vector a))

(define-hash-table-test 'sbw/value-eq-test 'sbw/value-eq 'sbw/value-eq-hashcode)

(provide 'sbw-value-eq)
