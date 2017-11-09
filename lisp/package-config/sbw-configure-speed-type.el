(require 'use-package)

(use-package speed-type
  :init
  (progn
    (setq speed-type-gb-book-list '(
                                    8492                 ;; Robert W Chambers
                                    50133                ;; H P Lovecraft
                                    3829 7028 8164 10554 ;; P G Wodehouse
                                    )
          speed-type-min-chars     400
          ))

  :config
  (progn)
)

(provide 'sbw-configure-speed-type)
