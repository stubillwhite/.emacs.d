(use-package mw-thesaurus
  :init
  (progn
    ;; (add-hook 'markdown-mode-hook 'mw-thesaurus-mode)
    )

  :config
  (progn
    (advice-add 'mw-thesaurus-lookup-at-point :around #'sbw/cosmetics-ensure-vertical-split-advice)

    (define-key mw-thesaurus-mode-map (kbd "q") 'delete-window)
    ;; (define-key mw-thesaurus-mode-map (kbd "<S-left>") 'windmove-left)
    ;; (define-key mw-thesaurus-mode-map (kbd "<S-right>") 'windmove-right)
    ;; (define-key mw-thesaurus-mode-map (kbd "<S-up>") 'windmove-up)
    ;; (define-key mw-thesaurus-mode-map (kbd "<S-down>") 'windmove-down)

    ;; TODO: Occasionally results in other buffers becoming read-only
    (defun sbw/open-thesaurus ()
      (interactive)
      (let ((curr-window (selected-window)))
        (mw-thesaurus-lookup-at-point (point))
        (sbw/open-and-switch-to-window mw-thesaurus-buffer-name)
        (select-window curr-window)))

    ;;    (defun sbw/open-thesaurus ()
    ;;      (interactive)
    ;;      (mw-thesaurus-lookup-at-point (point)))
    )

  :bind
  ("<f5>" . sbw/open-thesaurus))



