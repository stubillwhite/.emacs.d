(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)
(sml/apply-theme 'sbw-personal)
(sml/setup)

;; Hide minor modes that aren't of interest
(setq rm-blacklist '(" AC" " SP" " Fly" " Undo-Tree"))

(provide 'sbw-setup-smart-mode-line)
