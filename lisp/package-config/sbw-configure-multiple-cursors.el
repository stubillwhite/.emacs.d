(use-package multiple-cursors
  :defer t

  :init
  (progn)
  
  :config
  (progn
    (defun sbw/mc-mark-next-like-this-then-cycle-forward (arg)
      "Mark next like this then cycle forward."
      (interactive "p")
      (call-interactively 'mc/mark-next-like-this)
      (call-interactively 'mc/cycle-forward))

    (defun sbw/mc-skip-to-next-like-this-then-cycle-forward ()
      "Skip to next like this then cycle forward."
      (interactive)
      (mc/cycle-backward)
      (mc/skip-to-next-like-this)
      (mc/cycle-forward))

    (defun sbw/mc-mark-previous-like-this-then-cycle-backward ()
      "Mark previous like this then cycle backward."
      (interactive)
      (mc/mark-previous-like-this)
      (mc/cycle-backward))

    (defun sbw/mc-skip-to-previous-like-this-then-cycle-backward ()
      "Skip to previous like this then cycle backward."
      (interactive)
      (mc/cycle-forward)
      (mc/skip-to-previous-like-this)
      (mc/cycle-backward))
    )

  :bind
  ("C->" . sbw/mc-mark-next-like-this-then-cycle-forward)
  ("C-<" . sbw/mc-mark-previous-like-this-then-cycle-backward)
  ("C-." . mc/unmark-next-like-this)
  ("C-," . mc/unmark-previous-like-this)
  ("M-n" . mc/skip-to-next-like-this)
  )

(provide 'sbw-configure-mutiple-cursors)
