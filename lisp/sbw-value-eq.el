(require 'sbw-multimethods)

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

(defmacro sbw/value-eq--defmethod (type args equality-test)
  `(sbw/mm-defmethod sbw/value-eq ,type ,args
     (and
       (eq (sbw/value-eq--data-type (car args)) (sbw/value-eq--data-type (cadr args)))
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

(sbw/mm-defmulti sbw/value-eq 'sbw/value-eq--data-type)
(sbw/value-eq--defmethod [:nil]        (a b) (eq a b))
(sbw/value-eq--defmethod [:string]     (a b) (string-equal a b))
(sbw/value-eq--defmethod [:keyword]    (a b) (eq a b))
(sbw/value-eq--defmethod [:number]     (a b) (= a b))
(sbw/value-eq--defmethod [:hash-table] (a b) (sbw/value-eq--hash-tables-equal a b))
(sbw/value-eq--defmethod [:list]       (a b) (sbw/value-eq--lists-equal a b))
(sbw/value-eq--defmethod [:vector]     (a b) (sbw/value-eq--vectors-equal a b))

(defun sbw/value-eq--stringify-hash-table (a)
  (concat
    "{" 
    (-reduce-from 
      (lambda (acc v) (concat acc (sbw/value-eq--stringify v) (sbw/value-eq--stringify (gethash v a))))
      ""
      (hash-table-keys a))
    "}"))

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

(sbw/mm-defmulti sbw/value-eq--stringify 'sbw/value-eq--data-type)
(sbw/mm-defmethod sbw/value-eq--stringify [:nil]        (a) (format "%s" a))
(sbw/mm-defmethod sbw/value-eq--stringify [:string]     (a) (format "%s" a))
(sbw/mm-defmethod sbw/value-eq--stringify [:keyword]    (a) (format "%s" a))
(sbw/mm-defmethod sbw/value-eq--stringify [:number]     (a) (format "%s" a))
(sbw/mm-defmethod sbw/value-eq--stringify [:hash-table] (a) (sbw/value-eq--stringify-hash-table a))
(sbw/mm-defmethod sbw/value-eq--stringify [:list]       (a) (sbw/value-eq--stringify-list a))
(sbw/mm-defmethod sbw/value-eq--stringify [:vector]     (a) (sbw/value-eq--stringify-vector a))

(defun sbw/value-eq-hashcode (x)
  (sxhash (sbw/value-eq--stringify x)))

(define-hash-table-test 'sbw/value-eq-test 'sbw/value-eq 'sbw/value-eq-hashcode)

(provide 'sbw-value-eq)

