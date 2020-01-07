(use-package flyspell
  :diminish flyspell-mode
  :defer t

  :init
  (progn
    ;; Spell check all text modes, apart from markup modes
    (defun sbw/flyspell--enable-flyspell-mode ()
      (let* ( (non-flyspell-modes '(nxml-mode)) )
        (when (not (seq-contains non-flyspell-modes major-mode))
          (flyspell-mode 1))))
    
    (dolist (x '(text-mode-hook))
      (add-hook x 'sbw/flyspell--enable-flyspell-mode)))

  :config
  (progn
    (require 'ispell)
    
    ;; Aspell is slow, so use ultra mode to improve performance
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")
    
    (setq
      ispell-silently-savep      t         ;; Don't prompt when adding to the dictionary
      ispell-dictionary          "british" ;; British English
      )

    ;; Spelling program
    (when (sbw/is-darwin?)
      (setq ispell-program-name "/usr/local/bin/aspell"))
    
    ;; Personal dictionary location        
    (setq ispell-personal-dictionary
      (cond
        ((sbw/is-windows?) "/cygwin64/home/GB105549/.emacs.d/.ispell-win")
        ((sbw/is-linux?)   (concat user-emacs-directory ".ispell-unix"))
        ((sbw/is-darwin?)  (concat user-emacs-directory ".aspell-darwin"))))

    ;; Inverse of flyspell-goto-next-error, and tightly coupled to that implementation
    (defun sbw/flyspell-goto-previous-error ()
      "Go to the previous previously detected error."
      (interactive)
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto end of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (setq pos (1- pos)))
        ;; move back to the start of the word
        (goto-char pos)
        (setq pos (car (bounds-of-thing-at-point 'word)))
        ;; save the current location for next invocation
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
	    (message "No more miss-spelled word!"))))

  ;; TODO: Check wether this is required
  (defun sbw/ispell-configure-org-mode-ignored-regions ()
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src")))
  ;; (add-hook 'org-mode-hook 'sbw/ispell-configure-org-mode-ignored-regions)
    )


  :bind
  ("C-c s w" . flyspell-correct-at-point)
  ("C-c s b" . flyspell-buffer))

(provide 'sbw-configure-flyspell)
