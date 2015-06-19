(deftheme sbw-dark-muted "Dark muted theme.")

(defgroup sbw-dark-muted-faces nil
  "Faces used by sbw-dark-muted theme."
  :group 'faces)

(defun sbw/theme--create-face (name spec)
  (eval `(defface ,name ,spec "Face ,name." :group 'sbw-dark-muted-faces)))

(let* ( (*normal-bg*     "gray10")
        (*normal-fg*     "gray80")
        (*emphasis-bg*   "gray10")
        (*emphasis-fg*   "gray100")

        (*selection-bg*  "gray30")
        (*success-fg*    "DarkSeaGreen3")
        (*warning-fg*    "burlywood2")
        (*error-fg*      "IndianRed3")

        (*match-bg*      "darkslategray")
        (*match-fg*      "gray100")
        (*mismatch-bg*   "IndianRed3")
        (*mismatch-fg*   "gray100")
       
        (*item-1*        "DarkSeaGreen3")
        (*item-2*        "burlywood1")
        (*item-3*        "CadetBlue2")
        (*item-4*        "burlywood2")
        )

  (mapc
    (lambda (x) (apply 'sbw/theme--create-face x))
    `( (sbw-dark-muted-powerline-one-active      `((t (:foreground "gray100" :background "SkyBlue4"))))
       (sbw-dark-muted-powerline-one-inactive    `((t (:foreground "gray100" :background "gray30"))))
       (sbw-dark-muted-powerline-one-evil-insert `((t (:foreground "gray100" :background "coral3"))))
       (sbw-dark-muted-powerline-one-evil-normal `((t (:foreground "gray100" :background "coral4"))))
       (sbw-dark-muted-powerline-two             `((t (:foreground "gray100" :background "gray20"))))
       (sbw-dark-muted-powerline-three           `((t (:foreground "gray100" :background "gray15"))))

       (sbw-dark-muted-normal              `((t (:inherit default               :foreground ,*normal-fg*   :background ,*normal-bg*))))
       (sbw-dark-muted-selection           `((t (:inherit sbw-dark-muted-normal                            :background ,*selection-bg*))))
       (sbw-dark-muted-comment             `((t (:inherit sbw-dark-muted-normal :foreground ,*item-1*))))
       (sbw-dark-muted-string              `((t (:inherit sbw-dark-muted-normal :foreground ,*item-4*))))
       (sbw-dark-muted-success             `((t (:inherit sbw-dark-muted-normal :foreground ,*success-fg*))))
       (sbw-dark-muted-warning             `((t (:inherit sbw-dark-muted-normal :foreground ,*warning-fg*))))
       (sbw-dark-muted-error               `((t (:inherit sbw-dark-muted-normal :foreground ,*error-fg*))))    
       (sbw-dark-muted-emphasis            `((t (:inherit sbw-dark-muted-normal :foreground ,*emphasis-fg* :background ,*emphasis-bg*))))    
       (sbw-dark-muted-match               `((t (:inherit sbw-dark-muted-normal :foreground ,*match-fg*    :background ,*match-bg*))))
       (sbw-dark-muted-constant            `((t (:inherit sbw-dark-muted-normal :foreground ,*item-2*))))
       (sbw-dark-muted-mismatch            `((t (:inherit sbw-dark-muted-normal :foreground ,*mismatch-fg* :background ,*mismatch-bg*))))
       (sbw-dark-muted-keyword             `((t (:inherit sbw-dark-muted-normal :foreground ,*item-3*))))

       (sbw-dark-muted-menu                `((t (:inherit sbw-dark-muted-normal                            :background "gray30"))))
       (sbw-dark-muted-menu-selection      `((t (:inherit sbw-dark-muted-normal                            :background "SkyBlue4"))))
       ))

  (custom-theme-set-faces 'sbw-dark-muted

    `(default                             ((t (:background ,*normal-bg* :foreground ,*normal-fg*))))

    ;; Basics
    '(bold                                ((t (:inherit sbw-dark-muted-normal :bold t))))
    '(highlight                           ((t (:inherit sbw-dark-muted-match))))
    '(isearch                             ((t (:inherit sbw-dark-muted-match))))
    '(isearch-fail                        ((t (:inherit sbw-dark-muted-mismatch))))
    '(lazy-highlight                      ((t (:inherit sbw-dark-muted-match))))
    '(region                              ((t (:inherit sbw-dark-muted-selection))))
    '(show-paren-match                    ((t (:inherit sbw-dark-muted-match))))
    '(show-paren-mismatch                 ((t (:inherit sbw-dark-muted-mismatch))))
    '(underline                           ((t (:inherit sbw-dark-muted-normal :underline t))))

    ;; GUI
    '(cursor                              ((t (:inherit sbw-dark-muted-normal))))
    '(fringe                              ((t (:inherit sbw-dark-muted-normal))))
    '(minibuffer-prompt                   ((t (:inherit sbw-dark-muted-normal))))
    '(mode-line-inactive                  ((t (:inherit sbw-dark-muted-mode-line-inactive))))
    '(mode-line                           ((t (:inherit sbw-dark-muted-mode-line))))
    '(text-cursor                         ((t (:inherit sbw-dark-muted-normal))))
    '(vertical-border                     ((t (:inherit sbw-dark-muted-normal))))

    ;; company
    '(company-tooltip                     ((t (:inherit sbw-dark-muted-menu))))
    '(company-tooltip-common              ((t (:inherit sbw-dark-muted-menu :foreground "gray100"))))
    '(company-tooltip-common-selection    ((t (:inherit sbw-dark-muted-menu-selection :foreground "gray100"))))
    '(company-tooltip-selection           ((t (:inherit sbw-dark-muted-menu-selection))))
    
    ;; ert
    '(ert-test-result-expected            ((t (:inherit sbw-dark-muted-success))))
    '(ert-test-result-unexpected          ((t (:inherit sbw-dark-muted-error))))
            
    ;; eshell
    '(eshell-prompt                       ((t (:inherit sbw-dark-muted-keyword))))

    ;; expectations
    '(expectations-failure-face           ((t (:inherit sbw-dark-muted-error))))
    '(expectations-success-face           ((t (:inherit sbw-dark-muted-success))))
    '(expectations-error-face             ((t (:inherit sbw-dark-muted-warning))))

    ;; flyspell
    `(flyspell-duplicate-face ((t (:inherit sbw-dark-muted-normal :underline (:color ,*warning-fg* :style wave)))))
    `(flyspell-incorrect-face ((t (:inherit sbw-dark-muted-normal :underline (:color ,*error-fg*   :style wave)))))
    
    ;; font-lock
    '(font-lock-builtin-face              ((t (:inherit sbw-dark-muted-normal))))
    '(font-lock-comment-delimiter-face    ((t (:inherit sbw-dark-muted-comment))))
    '(font-lock-comment-face              ((t (:inherit sbw-dark-muted-comment))))
    '(font-lock-constant-face             ((t (:inherit sbw-dark-muted-constant))))
    '(font-lock-doc-face                  ((t (:inherit sbw-dark-muted-string))))
    '(font-lock-doc-string-face           ((t (:inherit sbw-dark-muted-string))))
    '(font-lock-function-name-face        ((t (:inherit sbw-dark-muted-keyword))))
    '(font-lock-keyword-face              ((t (:inherit sbw-dark-muted-keyword))))
    '(font-lock-negation-char-face        ((t (:inherit sbw-dark-muted-error))))
    '(font-lock-number-face               ((t (:inherit sbw-dark-muted-constant))))
    '(font-lock-preprocessor-face         ((t (:inherit sbw-dark-muted-keyword))))
    '(font-lock-reference-face            ((t (:inherit sbw-dark-muted-constant))))
    '(font-lock-regexp-grouping-backslash ((t (:inherit sbw-dark-muted-match))))
    '(font-lock-regexp-grouping-construct ((t (:inherit sbw-dark-muted-match))))
    '(font-lock-string-face               ((t (:inherit sbw-dark-muted-string))))
    '(font-lock-type-face                 ((t (:inherit sbw-dark-muted-normal))))
    '(font-lock-variable-name-face        ((t (:inherit sbw-dark-muted-keyword))))
    '(font-lock-warning-face              ((t (:inherit sbw-dark-muted-error))))

    ;; helm
    '(helm-match                          ((t (:inherit sbw-dark-muted-emphasis))))
    '(helm-grep-file                      ((t (:inherit sbw-dark-muted-normal))))
    '(helm-grep-lineno                    ((t (:inherit sbw-dark-muted-normal))))
    '(helm-grep-match                     ((t (:inherit sbw-dark-muted-emphasis))))    
    '(helm-ff-symlink                     ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-ff-invalid-symlink             ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-ff-directory                   ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-buffer-process                 ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-candidate-number               ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-selection                      ((t (:inherit sbw-dark-muted-menu-selection))))

    ;; helm-swoop
    '(helm-swoop-target-word-face         ((t (:inherit sbw-dark-muted-emphasis))))
    '(helm-swoop-target-line-face         ((t (:inherit sbw-dark-muted-selection))))
    '(helm-swoop-target-line-block-face   ((t (:inherit sbw-dark-muted-selection))))
    '(helm-source-header                  ((t (:inherit sbw-dark-muted-emphasis))))
        
    ;; nXML
    '(nxml-attribute-colon                ((t (:inherit sbw-dark-muted-normal))))
    '(nxml-attribute-local-name           ((t (:inherit sbw-dark-muted-normal))))
    '(nxml-attribute-prefix               ((t (:inherit sbw-dark-muted-normal))))
    '(nxml-element-local-name             ((t (:inherit sbw-dark-muted-keyword))))
    '(nxml-element-colon                  ((t (:inherit sbw-dark-muted-keyword))))
    '(nxml-element-prefix                 ((t (:inherit sbw-dark-muted-keyword))))
    '(nxml-delimiter                      ((t (:inherit sbw-dark-muted-normal))))
    '(nxml-attribute-value                ((t (:inherit sbw-dark-muted-constant))))
    '(nxml-attribute-value-delimiter      ((t (:inherit sbw-dark-muted-constant))))
    '(nxml-comment-content                ((t (:inherit sbw-dark-muted-comment))))
    '(nxml-comment-delimiter              ((t (:inherit sbw-dark-muted-comment))))

    ;; org
    '(org-agenda-clocking                 ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-agenda-current-time             ((t (:inherit sbw-dark-muted-normal))))
    '(org-agenda-date                     ((t (:inherit sbw-dark-muted-keyword))))
    '(org-agenda-done                     ((t (:inherit sbw-dark-muted-normal))))
    '(org-agenda-structure                ((t (:inherit sbw-dark-muted-keyword))))
    '(org-checkbox                        ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-date                            ((t (:inherit sbw-dark-muted-normal))))
    '(org-done                            ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-drawer                          ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-1                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-2                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-3                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-4                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-5                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-6                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-7                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-8                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-link                            ((t (:inherit sbw-dark-muted-normal :underline t))))
    '(org-priority                        ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-scheduled                       ((t (:inherit sbw-dark-muted-normal))))
    '(org-scheduled-previously            ((t (:inherit sbw-dark-muted-error))))
    '(org-scheduled-today                 ((t (:inherit sbw-dark-muted-normal))))
    '(org-special-keyword                 ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-time-grid                       ((t (:inherit sbw-dark-muted-keyword))))
    '(org-todo                            ((t (:inherit sbw-dark-muted-emphasis))))
    ))

(provide-theme 'sbw-dark-muted)
