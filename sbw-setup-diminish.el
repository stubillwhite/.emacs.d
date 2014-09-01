;; Hide minor mode indicators that aren't interesting

(eval-after-load "undo-tree"   '(diminish 'undo-tree-mode))
(eval-after-load "flyspell"    '(diminish 'flyspell-mode))
(eval-after-load "projectile"  '(diminish 'projectile-mode))
(eval-after-load "org-indent"  '(diminish 'org-indent-mode))

;;(eval-after-load "clojure-mode"   '(diminish 'clojure-mode (string 32 #x03BB)))

(provide 'sbw-setup-diminish)
