(require 'use-package)

(use-package clojure-mode
  :config
  (progn
    
;(eval-after-load 'clojure-mode
;  '(font-lock-add-keywords
;    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
;                     (0 (progn (compose-region (match-beginning 1)
;                                               (match-end 1) "λ")
;                               nil))))))

;(eval-after-load 'clojure-mode
;  '(font-lock-add-keywords
;    'clojure-mode `(("\\(#\\)("
;                     (0 (progn (compose-region (match-beginning 1)
;                                               (match-end 1) "ƒ")
;                               nil))))))

    ))

(provide 'sbw-setup-clojure-mode)
