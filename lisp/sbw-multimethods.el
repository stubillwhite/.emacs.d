(defun sbw/mm--set-registry (r)
  (setq *sbw/mm--registry* r))

(defun sbw/mm--get-registry ()
  (defvar *sbw/mm--registry* (sbw/ht-create))
  *sbw/mm--registry*)

(defun sbw/mm--register-dispatch-handler (f-symbol dispatch-val handler-f)
  (-> (sbw/mm--get-registry)
    (sbw/ht-assoc-in (list f-symbol dispatch-val) handler-f)
    (sbw/mm--set-registry))
  f-symbol)

(defun sbw/mm--register-multimethod (f)
  (-> (sbw/mm--get-registry)
    (sbw/ht-assoc f (sbw/ht-create))
    (sbw/mm--set-registry)))

(defun sbw/mm--get-handler (f-symbol dispatch-func args)
  (let* ( (dispatch-val (apply dispatch-func args)) )
    (-> (sbw/mm--get-registry)
      (sbw/ht-get-in (list f-symbol (vector dispatch-val))))))

(defmacro sbw/mm--create-multimethod-function (f-symbol dispatch-func)
  `(defun ,f-symbol (&rest args)
     (let* ( (handler-f (sbw/mm--get-handler (quote ,f-symbol) ,dispatch-func args)) )
       (apply handler-f args))))

(defmacro sbw/mm-defmulti (name dispatch-func)
  "Define NAME as a multimethod dispatched by DISPATCH-FUNC."
  (let ( (f-symbol (intern (symbol-name name))) )
    `(progn
       (sbw/mm--register-multimethod (quote ,f-symbol))
       (sbw/mm--create-multimethod-function ,f-symbol ,dispatch-func))))

(defmacro sbw/mm-defmethod (name dispatch-val sig body)
  "Define NAME as a multimethod for DISPATCH-VAL with the parameter signature SIG and body BODY."
  (let ( (f-symbol (intern (symbol-name name))) )
    `(sbw/mm--register-dispatch-handler (quote ,f-symbol) ,dispatch-val (lambda ,sig ,body))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parity (x &rest args)
  (if (= 0 (mod x 2)) :even :odd))

(setq *sbw/mm--registry* (sbw/ht-create))

(sbw/mm-defmulti describe-number 'parity)
(sbw/mm-defmethod describe-number [:even] (x y) (print (format "The number %d is even" x)))
(sbw/mm-defmethod describe-number [:odd]  (x y) (print (format "The number %d is odd" x)))

(describe-number 1 2)
(describe-number 2 2)

(print *sbw/mm--registry*)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Type-Predicates.html#Type-Predicates
(defun sbw/data-type (x &rest args)
  (cond
    ((stringp x)      :string)
    ((keywordp x)     :keyword)
    ((symbolp x)      :symbol)
    ((numberp x)      :number)
    ((hash-table-p x) :hash-table)
    ((listp x)        :list)
    ((vectorp x)      :vector)
    (t                (signal 'wrong-type-argument (list x)))))


(defun sbw/pprint (x)
  (sbw/-pprint-to-string x 0))

(defun sbw/-pprint-indent (s depth)
  (concat (s-repeat depth " ") s "\n"))

(defun sbw/-pprint-string-to-string (x depth)
  (sbw/-pprint-indent x depth))

(defun sbw/-pprint-keyword-to-string (x depth)
  (sbw/-pprint-indent (symbol-name x) depth))

(defun sbw/-pprint-number-to-string (x depth)
  (sbw/-pprint-indent (number-to-string x) depth))

(defun sbw/-pprint-vector-to-string (x depth)
  (concat
    (sbw/-pprint-indent "[" depth)
    (apply 'concat
      (-map (lambda (x) (sbw/-pprint-to-string x (+ 4 depth))) x))
    (sbw/-pprint-indent "]" depth)))

(defun sbw/-pprint-hash-table-to-string (x depth)
  (concat
    (sbw/-pprint-indent "{" depth)
    (-reduce-from
      (lambda (acc k) (concat
                   acc
                   (sbw/-pprint-to-string k (+ 4 depth))
                   ":"
                   (sbw/-pprint-to-string (sbw/ht-get x k) (+ 4 depth))))
      ""
      (sbw/ht-keys x))
    (sbw/-pprint-indent "}" depth)))

(sbw/mm-defmulti sbw/-pprint-to-string 'sbw/data-type)
(sbw/mm-defmethod sbw/-pprint-to-string [:string]     (x depth) (sbw/-pprint-string-to-string x depth))
(sbw/mm-defmethod sbw/-pprint-to-string [:keyword]    (x depth) (sbw/-pprint-keyword-to-string x depth))
(sbw/mm-defmethod sbw/-pprint-to-string [:number]     (x depth) (sbw/-pprint-number-to-string x depth))
(sbw/mm-defmethod sbw/-pprint-to-string [:hash-table] (x depth) (sbw/-pprint-hash-table-to-string x depth))
;(sbw/mm-defmethod sbw/-pprint-to-string [:list]       (x depth) (sbw/-pprint-number-to-string x depth))
(sbw/mm-defmethod sbw/-pprint-to-string [:vector]     (x depth) (sbw/-pprint-vector-to-string x depth))


(defun sbw/pprint (x)
  "Return X as a pretty-printed string."
  (let ( (json-encoding-pretty-print t) )
    (s-replace "\"" "" (json-encode x))))

(sbw/pprint (sbw/ht-create "foo" 23))



(sbw/pprint "foo")
(sbw/pprint :foo)
(sbw/pprint 23)
(sbw/pprint (sbw/ht-create :k1 :v1 :k2 (sbw/ht-create :k3 :v3)))
;(sbw/pprint (list 1 2 (list 3 4)))
(sbw/pprint [1 2 [3 4]])


(defun pprint (form &optional output-stream)
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
    output-stream))

(provide 'sbw-multimethods)
