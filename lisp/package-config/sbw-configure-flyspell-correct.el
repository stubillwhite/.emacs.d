(use-package flyspell-correct
  :config
  (progn
    (require 'flyspell-correct-ivy)
    (setq flyspell-correct-interface 'flyspell-correct-ivy))
  
  :bind
  ("C-;" . flyspell-correct-word-generic))

(provide 'sbw-configure-dash)
