;; File to store Emacs customisations to keep them out of my files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(org-agenda-files
   '((sbw/dropbox-subfolder "Private/org/current/kd/concept.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/consumption.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/data-quality.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/dkp-future.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/dkp.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/engineering-culture.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/helix.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/kd-kf-integration.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/kd-recruitment.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/knowledge-formation.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/line-management.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/meetings.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/ml-ops.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/pdp.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/personal-development.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/taxonomies.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/tech-associates.org")
     (sbw/dropbox-subfolder "Private/org/current/kd/women-in-tech.org")))
 '(package-selected-packages
   '(nil speed-type rainbow-mode queue org magit inflections groovy-mode flycheck evil dash-functional company))
 '(safe-local-variable-values
   '((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (intero-targets "Foo:lib" "Foo:exe:Foo" "Foo:test:Foo-test")
     (intero-targets "WhiteTest:test:WhiteTest-test"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
