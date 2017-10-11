(deftheme sbw-dark-muted "Dark muted theme.")

(defgroup sbw-dark-muted-faces nil
  "Faces used by sbw-dark-muted theme."
  :group 'faces)

(defun sbw/theme--create-face (name spec)
  (eval `(defface ,name ,spec "Face ,name." :group 'sbw-dark-muted-faces)))

(let* (
        (*normal-bg*         "gray10")
        (*normal-fg*         "gray80")
        (*emphasis-bg*       *normal-bg*)
        (*emphasis-fg*       "gray100")
        (*demphasis-bg*      *normal-bg*)
        (*demphasis-fg*      "gray50")

        (*selection-bg*      "gray30")
        (*success-fg*        "DarkSeaGreen3")
        (*warning-fg*        "burlywood2")
        (*error-fg*          "Coral4")

        (*match-bg*          "darkslategray")
        (*match-fg*          "gray100")
        (*mismatch-bg*       "Coral4")
        (*mismatch-fg*       "gray100")
       
        (*item-1*            "DarkSeaGreen3")
        (*item-2*            "burlywood1")
        (*item-3*            "CadetBlue3")
        (*item-4*            "burlywood2")

        (*ui-bg-active*      "SkyBlue4")
        (*ui-bg-inactive*    "gray30")
                
        (*menu-bg*           *ui-bg-inactive*)
        (*menu-selection-bg* *ui-bg-active*)

        (*modeline-fg*          *emphasis-fg*)
        (*modeline-bg-active*   *ui-bg-active*) 
        (*modeline-bg-inactive* *ui-bg-inactive*)        
        )

  (mapc
    (lambda (x) (apply 'sbw/theme--create-face x))
    `( (sbw-dark-muted-powerline-one-active      `((t (:foreground ,*modeline-fg* :background ,*modeline-bg-active*))))
       (sbw-dark-muted-powerline-one-inactive    `((t (:foreground ,*modeline-fg* :background ,*modeline-bg-inactive*))))
       (sbw-dark-muted-powerline-two             `((t (:foreground ,*modeline-fg* :background ,(color-darken-name *modeline-bg-inactive* 8)))))
       (sbw-dark-muted-powerline-three           `((t (:foreground ,*modeline-fg* :background ,(color-darken-name *modeline-bg-inactive* 15)))))

       (sbw-dark-muted-normal              `((t (:inherit default               :foreground ,*normal-fg*   :background ,*normal-bg*))))
       (sbw-dark-muted-selection           `((t (:inherit sbw-dark-muted-normal                            :background ,*selection-bg*))))
       (sbw-dark-muted-comment             `((t (:inherit sbw-dark-muted-normal :foreground ,*item-1*))))
       (sbw-dark-muted-string              `((t (:inherit sbw-dark-muted-normal :foreground ,*item-4*))))
       (sbw-dark-muted-success             `((t (:inherit sbw-dark-muted-normal :foreground ,*success-fg*))))
       (sbw-dark-muted-warning             `((t (:inherit sbw-dark-muted-normal :foreground ,*warning-fg*))))
       (sbw-dark-muted-error               `((t (:inherit sbw-dark-muted-normal :foreground ,*error-fg*))))    
       (sbw-dark-muted-emphasis            `((t (:inherit sbw-dark-muted-normal :foreground ,*emphasis-fg* :background ,*emphasis-bg*))))    
       (sbw-dark-muted-demphasis           `((t (:inherit sbw-dark-muted-normal :foreground ,*demphasis-fg* :background ,*demphasis-bg*))))    
       (sbw-dark-muted-match               `((t (:inherit sbw-dark-muted-normal :foreground ,*match-fg*    :background ,*match-bg*))))
       (sbw-dark-muted-constant            `((t (:inherit sbw-dark-muted-normal :foreground ,*item-2*))))
       (sbw-dark-muted-mismatch            `((t (:inherit sbw-dark-muted-normal :foreground ,*mismatch-fg* :background ,*mismatch-bg*))))
       (sbw-dark-muted-keyword             `((t (:inherit sbw-dark-muted-normal :foreground ,*item-3*))))

       (sbw-dark-muted-menu                `((t (:inherit sbw-dark-muted-normal                            :background ,*menu-bg*))))
       (sbw-dark-muted-menu-selection      `((t (:inherit sbw-dark-muted-normal                            :background ,*menu-selection-bg*))))
       ))

  (custom-theme-set-faces 'sbw-dark-muted

    `(default                             ((t (:background ,*normal-bg* :foreground ,*normal-fg*))))

    ;; Basics
    `(bold                                ((t (:inherit sbw-dark-muted-normal :bold t))))
    `(highlight                           ((t (:inherit sbw-dark-muted-match))))
    `(isearch                             ((t (:inherit sbw-dark-muted-match))))
    `(isearch-fail                        ((t (:inherit sbw-dark-muted-mismatch))))
    `(lazy-highlight                      ((t (:inherit sbw-dark-muted-match))))
    `(match                               ((t (:inherit sbw-dark-muted-match))))
    `(region                              ((t (:inherit sbw-dark-muted-selection))))
    `(show-paren-match                    ((t (:inherit sbw-dark-muted-match))))
    `(show-paren-mismatch                 ((t (:inherit sbw-dark-muted-mismatch))))
    `(underline                           ((t (:inherit sbw-dark-muted-normal :underline t))))
    `(link                                ((t (:inherit sbw-dark-muted-keyword :underline t))))
    `(link-visited                        ((t (:inherit sbw-dark-muted-normal :underline t))))
    
    ;; GUI
    `(cursor                              ((t (:inherit sbw-dark-muted-normal))))
    `(fringe                              ((t (:inherit sbw-dark-muted-normal))))
    `(minibuffer-prompt                   ((t (:inherit sbw-dark-muted-normal))))
    `(mode-line-inactive                  ((t (:inherit sbw-dark-muted-mode-line-inactive))))
    `(mode-line                           ((t (:inherit sbw-dark-muted-mode-line))))
    `(text-cursor                         ((t (:inherit sbw-dark-muted-normal))))
    `(vertical-border                     ((t (:inherit sbw-dark-muted-normal))))

    ;; avy
    `(avy-lead-face                       ((t (:inherit sbw-dark-muted-emphasis))))
    `(avy-lead-face-0                     ((t (:inherit sbw-dark-muted-emphasis))))
    `(avy-lead-face-1                     ((t (:inherit sbw-dark-muted-emphasis))))
    `(avy-lead-face-2                     ((t (:inherit sbw-dark-muted-emphasis))))
        
    ;; company
    `(company-tooltip                       ((t (:inherit sbw-dark-muted-menu))))
    `(company-tooltip-common                ((t (:inherit sbw-dark-muted-menu :foreground ,*emphasis-fg*))))
    `(company-tooltip-common-selection      ((t (:inherit sbw-dark-muted-menu-selection :foreground ,*emphasis-fg*))))
    `(company-tooltip-annotation            ((t (:inherit sbw-dark-muted-menu))))
    `(company-tooltip-annotation-selection  ((t (:inherit sbw-dark-muted-menu-selection :foreground ,*emphasis-fg*))))
    `(company-tooltip-selection             ((t (:inherit sbw-dark-muted-menu-selection))))
    `(company-scrollbar-bg                  ((t (:inherit sbw-dark-muted-menu))))
    `(company-scrollbar-fg                  ((t (:inherit sbw-dark-muted-menu-selection))))

    ;; compile
    `(compilation-info                    ((t (:inherit sbw-dark-muted-emphasis))))
    
    ;; ert
    `(ert-test-result-expected            ((t (:inherit sbw-dark-muted-success))))
    `(ert-test-result-unexpected          ((t (:inherit sbw-dark-muted-error))))
            
    ;; eshell
    `(eshell-prompt                       ((t (:inherit sbw-dark-muted-keyword))))

    ;; expectations
    `(expectations-failure-face           ((t (:inherit sbw-dark-muted-error))))
    `(expectations-success-face           ((t (:inherit sbw-dark-muted-success))))
    `(expectations-error-face             ((t (:inherit sbw-dark-muted-warning))))

    ;; flycheck
    `(flycheck-warning                    ((t (:inherit sbw-dark-muted-normal :underline (:color ,*warning-fg* :style wave)))))
    `(flycheck-fringe-warning             ((t (:inherit sbw-dark-muted-warning))))
    `(flycheck-error-list-warning         ((t (:inherit sbw-dark-muted-normal :underline (:color ,*error-fg* :style wave)))))
    `(flycheck-fringe-error               ((t (:inherit sbw-dark-muted-error))))
    `(flycheck-error-list-error           ((t (:inherit sbw-dark-muted-error))))
    `(flycheck-error-list-line-number     ((t (:inherit sbw-dark-muted-normal))))
    `(flycheck-error-list-column-number   ((t (:inherit sbw-dark-muted-normal))))
  
    ;; flyspell
    `(flyspell-duplicate-face             ((t (:inherit sbw-dark-muted-normal :underline (:color ,*warning-fg* :style wave)))))
    `(flyspell-incorrect-face             ((t (:inherit sbw-dark-muted-normal :underline (:color ,*error-fg*   :style wave)))))
    
    ;; font-lock
    `(font-lock-builtin-face              ((t (:inherit sbw-dark-muted-normal))))
    `(font-lock-comment-delimiter-face    ((t (:inherit sbw-dark-muted-comment))))
    `(font-lock-comment-face              ((t (:inherit sbw-dark-muted-comment))))
    `(font-lock-constant-face             ((t (:inherit sbw-dark-muted-constant))))
    `(font-lock-doc-face                  ((t (:inherit sbw-dark-muted-string))))
    `(font-lock-doc-string-face           ((t (:inherit sbw-dark-muted-string))))
    `(font-lock-function-name-face        ((t (:inherit sbw-dark-muted-keyword))))
    `(font-lock-keyword-face              ((t (:inherit sbw-dark-muted-keyword))))
    `(font-lock-negation-char-face        ((t (:inherit sbw-dark-muted-normal))))
    `(font-lock-number-face               ((t (:inherit sbw-dark-muted-constant))))
    `(font-lock-preprocessor-face         ((t (:inherit sbw-dark-muted-keyword))))
    `(font-lock-reference-face            ((t (:inherit sbw-dark-muted-constant))))
    `(font-lock-regexp-grouping-backslash ((t (:inherit sbw-dark-muted-match))))
    `(font-lock-regexp-grouping-construct ((t (:inherit sbw-dark-muted-match))))
    `(font-lock-string-face               ((t (:inherit sbw-dark-muted-string))))
    `(font-lock-type-face                 ((t (:inherit sbw-dark-muted-normal))))
    `(font-lock-variable-name-face        ((t (:inherit sbw-dark-muted-keyword))))
    `(font-lock-warning-face              ((t (:inherit sbw-dark-muted-error))))

    ;; helm
    `(helm-visible-mark                   ((t (:inherit sbw-dark-muted-match))))
    `(helm-buffer-directory               ((t (:inherit sbw-dark-muted-warning))))
    `(helm-buffer-not-saved               ((t (:inherit sbw-dark-muted-error))))
    `(helm-buffer-process                 ((t (:inherit sbw-dark-muted-keyword))))
    `(helm-buffer-saved-out               ((t (:inherit sbw-dark-muted-error))))
    `(helm-candidate-number               ((t (:inherit sbw-dark-muted-keyword))))
    `(helm-ff-directory                   ((t (:inherit sbw-dark-muted-keyword))))
    `(helm-ff-dotted-directory            ((t (:inherit sbw-dark-muted-keyword))))
    `(helm-ff-executable                  ((t (:inherit sbw-dark-muted-normal))))
    `(helm-ff-invalid-symlink             ((t (:inherit sbw-dark-muted-keyword))))
    `(helm-ff-symlink                     ((t (:inherit sbw-dark-muted-keyword))))
    `(helm-grep-file                      ((t (:inherit sbw-dark-muted-normal))))
    `(helm-grep-lineno                    ((t (:inherit sbw-dark-muted-normal))))
    `(helm-grep-match                     ((t (:inherit sbw-dark-muted-emphasis))))    
    `(helm-match                          ((t (:inherit sbw-dark-muted-emphasis))))
    `(helm-selection                      ((t (:inherit sbw-dark-muted-menu-selection))))

    ;; helm-swoop
    `(helm-swoop-target-word-face         ((t (:inherit sbw-dark-muted-emphasis))))
    `(helm-swoop-target-line-face         ((t (:inherit sbw-dark-muted-selection))))
    `(helm-swoop-target-line-block-face   ((t (:inherit sbw-dark-muted-selection))))
    `(helm-source-header                  ((t (:inherit sbw-dark-muted-emphasis))))
    
    `(hydra-face-blue                     ((t (:inherit sbw-dark-muted-keyword))))
    `(hydra-face-red                      ((t (:inherit sbw-dark-muted-error))))

    ;; Markdown
    `(markdown-code-face                  ((t (:inherit sbw-dark-muted-constant))))
    
    ;; nXML
    `(nxml-attribute-colon                ((t (:inherit sbw-dark-muted-normal))))
    `(nxml-attribute-local-name           ((t (:inherit sbw-dark-muted-normal))))
    `(nxml-attribute-prefix               ((t (:inherit sbw-dark-muted-normal))))
    `(nxml-element-local-name             ((t (:inherit sbw-dark-muted-keyword))))
    `(nxml-element-colon                  ((t (:inherit sbw-dark-muted-keyword))))
    `(nxml-element-prefix                 ((t (:inherit sbw-dark-muted-keyword))))
    `(nxml-delimiter                      ((t (:inherit sbw-dark-muted-normal))))
    `(nxml-attribute-value                ((t (:inherit sbw-dark-muted-constant))))
    `(nxml-attribute-value-delimiter      ((t (:inherit sbw-dark-muted-constant))))
    `(nxml-comment-content                ((t (:inherit sbw-dark-muted-comment))))
    `(nxml-comment-delimiter              ((t (:inherit sbw-dark-muted-comment))))

    ;; org
    `(org-agenda-clocking                 ((t (:inherit sbw-dark-muted-emphasis))))
    `(org-agenda-current-time             ((t (:inherit sbw-dark-muted-normal))))
    `(org-agenda-date                     ((t (:inherit sbw-dark-muted-keyword))))
    `(org-agenda-done                     ((t (:inherit sbw-dark-muted-normal))))
    `(org-agenda-structure                ((t (:inherit sbw-dark-muted-keyword))))
    `(org-checkbox                        ((t (:inherit sbw-dark-muted-emphasis))))
    `(org-date                            ((t (:inherit sbw-dark-muted-normal))))
    `(org-done                            ((t (:inherit sbw-dark-muted-emphasis :strike-through t))))
    `(org-headline-done                   ((t (:inherit sbw-dark-muted-normal :strike-through t))))
    `(org-drawer                          ((t (:inherit sbw-dark-muted-normal))))
    `(org-ellipsis                        ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-1                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-2                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-3                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-4                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-5                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-6                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-7                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-level-8                         ((t (:inherit sbw-dark-muted-normal))))
    `(org-link                            ((t (:inherit sbw-dark-muted-normal :underline t))))
    `(org-priority                        ((t (:inherit sbw-dark-muted-emphasis))))
    `(org-scheduled                       ((t (:inherit sbw-dark-muted-normal))))
    `(org-scheduled-previously            ((t (:inherit sbw-dark-muted-error))))
    `(org-upcoming-deadline               ((t (:inherit sbw-dark-muted-warning))))
    `(org-scheduled-today                 ((t (:inherit sbw-dark-muted-normal))))
    `(org-special-keyword                 ((t (:inherit sbw-dark-muted-emphasis))))
    `(org-time-grid                       ((t (:inherit sbw-dark-muted-keyword))))
    `(org-todo                            ((t (:inherit sbw-dark-muted-emphasis))))

    ;; scala
    `(scala-font-lock:abstract-face       ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:final-face          ((t (:inherit sbw-dark-muted-constant))))
    `(scala-font-lock:implicit-face       ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:lazy-face           ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:override-face       ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:private-face        ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:protected-face      ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:sealed-face         ((t (:inherit sbw-dark-muted-constant))))
    `(scala-font-lock:var-face            ((t (:inherit sbw-dark-muted-normal))))
    `(scala-font-lock:var-keyword-face    ((t (:inherit sbw-dark-muted-keyword))))
    
    ;; sh
    `(sh-heredoc                          ((t (:inherit sbw-dark-muted-string))))
    `(sh-quoted-exec                      ((t (:inherit sbw-dark-muted-keyword))))

    ;; slack
    `(lui-time-stamp-face                 ((t (:inherit sbw-dark-muted-keyword))))
    `(lui-strong-face                     ((t (:inherit sbw-dark-muted-normal))))
    `(lui-button-face                     ((t (:inherit sbw-dark-muted-demphasis))))
    `(slack-attachment-footer             ((t (:inherit sbw-dark-muted-demphasis))))
    `(slack-message-deleted-face          ((t (:inherit sbw-dark-muted-error))))
    `(slack-message-output-header         ((t (:inherit sbw-dark-muted-keyword))))
    `(slack-message-output-reaction       ((t (:inherit sbw-dark-muted-normal))))
    `(slack-message-output-text           ((t (:inherit sbw-dark-muted-normal))))
    `(slack-me ((t (:inherit sbw-dark-muted-normal))))
 
    ;; Smartparens
    `(sp-show-pair-match-face             ((t (:inherit sbw-dark-muted-match))))
    `(sp-show-pair-mismatch-face          ((t (:inherit sbw-dark-muted-mismatch))))

    ;; Terminal
    `(term-color-black                    ((t (:foreground ,*normal-bg*))))
    `(term-color-blue                     ((t (:foreground "SkyBlue4"))))
    `(term-color-cyan                     ((t (:foreground "CadetBlue3"))))
    `(term-color-green                    ((t (:foreground "DarkSeaGreen3"))))
    `(term-color-magenta                  ((t (:foreground "Coral3"))))
    `(term-color-red                      ((t (:foreground "Coral4"))))
    `(term-color-white                    ((t (:foreground ,*normal-fg*))))
    `(term-color-yellow                   ((t (:foreground "burlywood2"))))
    
    ;; whitespace
    `(whitespace-empty                    ((t (:inherit sbw-dark-muted-demphasis :underline (:color ,*warning-fg* :style wave)))))
    `(whitespace-line                     ((t (:inherit sbw-dark-muted-demphasis :underline (:color ,*warning-fg* :style wave)))))
    `(whitespace-indentation              ((t (:inherit sbw-dark-muted-demphasis :underline (:color ,*warning-fg* :style wave)))))
    `(whitespace-space                    ((t (:inherit sbw-dark-muted-demphasis))))
    `(whitespace-newline                  ((t (:inherit sbw-dark-muted-demphasis))))
    `(whitespace-trailing                 ((t (:inherit sbw-dark-muted-demphasis :underline (:color ,*warning-fg* :style wave)))))
    ))

(provide-theme 'sbw-dark-muted)
