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

(defun sbw/mm--unregister-multimethod (f)
  (-> (sbw/mm--get-registry)
    (sbw/ht-dissoc f)
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

(defmacro sbw/mm-undefmulti (name)
  "Undefine NAME as a multimethod."
  (let ( (f-symbol (intern (symbol-name name))) )
    `(progn
       (sbw/mm--unregister-multimethod (quote ,f-symbol)))))

(provide 'sbw-multimethods)
