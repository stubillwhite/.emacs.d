(require 'use-package)

(use-package evil-leader
  :init
  (progn
    (global-evil-leader-mode)
   
    (evil-leader/set-leader "<SPC>")

    ;; Windows
    (evil-leader/set-key
      "wh" 'evil-window-left
      "wj" 'evil-window-down
      "wk" 'evil-window-up
      "wl" 'evil-window-right
      "wH" 'evil-window-move-far-left
      "wJ" 'evil-window-move-very-bottom
      "wK" 'evil-window-move-very-top
      "wL" 'evil-window-move-far-right
      "wq" 'delete-window
      "wo" 'other-window
      "ws" 'split-window-below
      "wS" 'split-window-below-and-focus
      "wv" 'split-window-right
      "wV" 'split-window-right-and-focus
      "w=" 'balance-windows)

    ;; org-mode
    (evil-leader/set-key-for-mode 'org-mode
      "ml" 'org-open-at-point
      "mi" 'org-clock-in
      "mo" 'org-clock-out)
    )
  
  :config
  (progn))

(provide 'sbw-configure-evil-leader)
