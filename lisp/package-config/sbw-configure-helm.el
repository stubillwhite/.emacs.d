(use-package helm
  :diminish helm-mode
  
  :init
  (progn
    (require 'helm-projectile)
    (helm-projectile-on)

    (require 'helm-grep)

    ;; For some reason, helm-projectile-grep seems to reference this unset variable. Need to find out where this should
    ;; actually come from and why it is unset, but defining this now works.
    (setq helm-grep-mode-line-string nil)

    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; TAB to do persistent action
    (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; Ensure TAB works in terminal
    (define-key helm-map (kbd "C-z")   'helm-select-action)             ; List actions

    (setq
      helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-truncate-lines                   t
      )

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-F") 'helm-do-grep-ag)

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

    ;; C-backspace should kill word instead of toggling autoexpansion
    (define-key helm-find-files-map           (kbd "<C-backspace>") 'backward-kill-word)
    (define-key helm-projectile-find-file-map (kbd "<C-backspace>") 'backward-kill-word)
    
    (helm-mode 1)))

(provide 'sbw-configure-helm)
