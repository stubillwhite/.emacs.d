(use-package intero
  :config
  (progn
    (add-hook 'haskell-mode-hook 'intero-mode)
    
    (setq flycheck-check-syntax-automatically '(save new-line))
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

    (defun sbw/intero--ensure-vertical-split (orig-fun &rest args)
      (let* ( (orig-width  split-width-threshold)
              (orig-height split-height-threshold) )
        (save-some-buffers t)
        (setq split-width-threshold  0
              split-height-threshold nil)    
        (apply orig-fun args)
        (setq split-width-threshold  orig-width
              split-height-threshold orig-height)))
    (advice-add 'intero-repl :around #'sbw/intero--ensure-vertical-split)


    ;; (defun sbw/intero--current-declaration ()
    ;;   (save-excursion
    ;;     (goto-char (1+ (point)))
    ;;     (let* ( (start      (or (haskell-ds-backward-decl) (point-min)))
    ;;             (end        (or (haskell-ds-forward-decl) (point-max)))
    ;;             (raw-decl   (s-trim-right (buffer-substring start end)))
    ;;             (lines      (split-string raw-decl "\n"))
    ;;             (first-line (car lines)) )

    ;;       (inferior-haskell-flash-decl start end elm-flash-duration)
    ;;       (if (string-match-p "^[a-z].*:" first-line)
    ;;           (cdr lines)
    ;;         lines))))


    )


  )

(provide 'sbw-configure-intero)
