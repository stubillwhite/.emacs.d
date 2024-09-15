(use-package flyspell-correct
  :config
  (progn
    (require 'flyspell-correct-helm)
    (setq flyspell-correct-interface 'flyspell-correct-helm))
  
  :bind
  ("C-;" . flyspell-correct-word-generic))

(provide 'sbw-configure-flyspell-correct)
