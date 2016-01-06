(require 'use-package)

(use-package highlight-symbol
  :init
  (progn)

  :config
  (progn
    ;; (global-set-key [(control f3)] 'highlight-symbol)
    ;; (global-set-key [f3] 'highlight-symbol-next)
    ;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
    ;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)

    (setq highlight-symbol-colors '(sbw-dark-muted-match)))

  (bind-keys
   ("C-<f3>"  . highlight-symbol)
   ("<f3>"    . highlight-symbol-next)
   ("S-<f3>"  . highlight-symbol-prev)
   ("M-<f3>"  . highlight-symbol-query-replace)
   ))
