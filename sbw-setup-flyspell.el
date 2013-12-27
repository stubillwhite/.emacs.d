(require 'ispell)

;; Modes to spell check
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; We're using aspell, so use ultra mode to improve performance
(add-to-list 'ispell-extra-args "--sug-mode=ultra")

(provide 'sbw-setup-flyspell)
