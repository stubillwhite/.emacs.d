(require 'use-package)

(use-package flyspell
  :diminish flyspell-mode
  :defer t

  :init
  (progn
    ;; Spell check all text modes
    (dolist (x '(text-mode-hook))
      (add-hook x (lambda () (flyspell-mode 1)))))

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
        ((sbw/is-linux?)   "~/.emacs.d/.ispell-unix")
        ((sbw/is-darwin?)  "~/.emacs.d/.aspell-darwin"))))

  (defun sbw/ispell-configure-org-mode-ignored-regions ()
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src")))
  ;; (add-hook 'org-mode-hook 'sbw/ispell-configure-org-mode-ignored-regions)

  :bind
  ("C-c s w" . ispell-word)
  ("C-c s b" . flyspell-buffer))

(provide 'sbw-configure-flyspell)
