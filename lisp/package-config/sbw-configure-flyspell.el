(require 'use-package)

(use-package flyspell
  :diminish flyspell-mode
  :defer t

  :init
  (progn
    ;; Spell check all text modes
    (dolist (x '(text-mode-hook))
      (add-hook x (lambda () (flyspell-mode 1)))))

  :config
  (progn
    (require 'ispell)
    
    ;; Aspell is slow, so use ultra mode to improve performance
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")
    
    (setq
      ispell-silently-savep      t           ;; Don't prompt when adding to the dictionary
      ispell-personal-dictionary "~/.ispell" ;; Personal dictionary location
      ispell-dictionary          "british"   ;; British English
      ))

  :bind
  ("C-c s w" . ispell-word)
  ("C-c s b" . flyspell-buffer))

(provide 'sbw-configure-flyspell)
