(require 'use-package)

(use-package smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/apply-theme 'sbw-personal)
    (sml/setup)

    ;; Hide minor modes that aren't of interest
    (setq rm-blacklist '(" SP"" SP/s" " Fly" " Undo-Tree" " Helm" " Ind" " company" " yas" " OrgTbl" " Abbrev" " MRev"))))

(provide 'sbw-configure-smart-mode-line)
