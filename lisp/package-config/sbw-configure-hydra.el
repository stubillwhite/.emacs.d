(require 'use-package)

(use-package hydra
  :defer t

  :init
  (progn)

  :config
  (progn
    (defun hydra-vi/pre ()
      (set-cursor-color "#e52b50"))

    (defun hydra-vi/post ()
      (set-cursor-color "#ffffff"))

    (global-set-key
      (kbd "<f2>")
      (defhydra hydra-vi (:pre hydra-vi/pre :post hydra-vi/post :color amaranth)
        "vi"
        ("l" forward-char)
        ("h" backward-char)
        ("j" next-line)
        ("k" previous-line)
        ("m" set-mark-command "mark")
        ("a" move-beginning-of-line "beg")
        ("e" move-end-of-line "end")
        ("d" delete-region "del" :color blue)
        ("y" kill-ring-save "yank" :color blue)
        ("q" nil "quit")))))

(provide 'sbw-configure-hydra)
