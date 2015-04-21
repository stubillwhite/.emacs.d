(require 'use-package)

(use-package flyspell
  :diminish flyspell-mode
  
  :init
  (progn

    (require 'ispell)
    
    ;; Spell check all text modes
    (dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

    ;; Aspell is slow, so use ultra mode to improve performance
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")
    
    (setq
      ispell-silently-savep      t ;; Don't prompt when adding to the dictionary
      ispell-personal-dictionary "~/.emacs.d/.ispell"
      ))

  :bind
  ( ("C-c s w" . ispell-word)
    ("C-c s b" . flyspell-buffer) ))

(provide 'sbw-configure-flyspell)
