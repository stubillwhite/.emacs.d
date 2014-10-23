(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)
(sml/apply-theme 'sbw-personal)
(sml/setup)

;; Hide minor modes that aren't of interest
(setq rm-blacklist '(" AC" " SP"" SP/s" " Fly" " Undo-Tree" " Helm" " Ind"))

(provide 'sbw-setup-smart-mode-line)
