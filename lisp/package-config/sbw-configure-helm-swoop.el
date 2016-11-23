(require 'use-package)

(use-package helm-swoop

  :init
  (progn)

  :config
  (progn
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    (setq
     helm-swoop-split-direction      'split-window-horizontally ;; Vertical window
     helm-multi-swoop-edit-save      t                          ;; Save buffer when helm-multi-swoop-edit complete
     helm-swoop-speed-or-color       t                          ;; Sacrifice speed for color
     helm-swoop-use-line-number-face t                          ;; Use special face for line numbers
     helm-swoop-split-with-multiple-windows nil                 ;; Don't split inside multiple windows
     )))


(provide 'sbw-configure-helm-swoop)

