(use-package magit
  :defer t

  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"))
  
  :config
  (progn
    ;; Point to git.exe in Windows systems for a more responsive Magit
    (if (sbw/is-windows?)
        (setq magit-git-executable "/usr/bin/git.exe"))   


    (defun magit-diff-master (&optional args)
      "Show diff range master...HEAD"
      (interactive (list (magit-diff-read-args)))
      (magit-diff "master...HEAD" args))

    (defun magit-diff-mbase (&optional args)
      "Show diff of $(git merge-base master HEAD) to working tree."
      (interactive (list (magit-diff-read-args)))
      (magit-diff-working-tree
       (magit-git-string "merge-base" "master" "HEAD") args))

    (magit-define-popup-action 'magit-diff-popup
      ?m "Diff merge-base master" 'magit-diff-mbase)

    )

  :bind
  ("C-c g s" . magit-status))

(provide 'sbw-configure-magit)

