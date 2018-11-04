(use-package intero
  :diminish t
  
  :init
  (progn
    (add-hook 'haskell-mode-hook 'intero-mode t))
  
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save new-line))
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

    (setq intero-stack-executable "/usr/local/bin/stack")
    
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
    (advice-add 'intero-repl-load :around #'sbw/intero--ensure-vertical-split)

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

    (haskell-decl-scan-mode)

    ;; TODO: Only seems to work when on the type declaration itself
    (defun sbw/haskell-send-current-declaration ()
      (interactive)
      (save-excursion
        (goto-char (1+ (point)))
        (let* ( (start      (or (haskell-ds-backward-decl) (point-min)))
                (end        (or (haskell-ds-forward-decl) (point-max)))
                (raw-decl   (s-trim-right (buffer-substring start end)))
                (lines      (split-string raw-decl "\n"))
                (first-line (car lines))
                (decl       (concat "let "
                                    (s-join "\n    " lines)
                                    "\n")))
          (message (concat "Sending " decl))
          (intero-with-repl-buffer nil
            (comint-simple-send
             (get-buffer-process (current-buffer))
             decl)))))


    (defun sbw/intero-indent-and-complete-symbol ()
      "Indent the current line and perform symbol completion.
First indent the line.  If indenting doesn't move point, complete
the symbol."
      (interactive)
      (let ((pos (point)))
        (haskell-indentation-indent-line)
        (when (= pos (point))
          (if (save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
              (completion-at-point)))))

    )

  :bind
  ("C-c C-c" . sbw/haskell-send-current-declaration)
  ;; ("TAB" . sbw/intero-indent-and-complete-symbol)
  ("<f5>" . intero-repl-load))

(provide 'sbw-configure-intero)
