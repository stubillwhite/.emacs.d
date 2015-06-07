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
               (item-a (sbw/ht-get a key-a))
               (item-b (sbw/ht-get b key-b))) 
        (and acc (sbw/value-eq key-a key-b) (sbw/value-eq item-a item-b))))
    t
    (-zip-fill :sbw/value-eq--padding (sbw/ht-keys a) (sbw/ht-keys b))))

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

(provide 'sbw-value-eq)