(require 'use-package)

(use-package flyspell-correct
  :config
  (progn
    (setq flyspell-correct-interface 'flyspell-correct-helm))
  :bind
  ("C-;" . flyspell-correct-word-generic))

(provide 'sbw-configure-dash)
