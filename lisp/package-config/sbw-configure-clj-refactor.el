(use-package clj-refactor
  :defer t
  :diminish clj-refactor-mode
  
  :init
  (progn
    (add-hook 'clojure-mode-hook 'clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c C-m")
    (setq cljr-inject-dependencies-at-jack-in nil))

  :config
  (progn))

(provide 'sbw-clj-refactor)
