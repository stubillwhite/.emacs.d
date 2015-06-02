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
        (*warning-fg*    "burlywood1")
        (*error-fg*      "IndianRed3")

        (*match-bg*      "darkslategray")
        (*match-fg*      "gray100")
        (*mismatch-bg*   "IndianRed1")
        (*mismatch-fg*   "gray100")
       
        (*item-1*        "DarkSeaGreen3")
        (*item-2*        "burlywood1")
        (*item-3*        "CadetBlue2")
        (*item-4*        "burlywood2")
        )

  (mapc
    (lambda (x) (apply 'sbw/theme--create-face x))
    `( (sbw-dark-powerline-one-active      `((t (:foreground "gray100" :background "SkyBlue4"))))
       (sbw-dark-powerline-one-inactive    `((t (:foreground "gray100" :background "gray30"))))
       (sbw-dark-powerline-one-evil-insert `((t (:foreground "gray100" :background "coral3"))))
       (sbw-dark-powerline-one-evil-normal `((t (:foreground "gray100" :background "coral4"))))
       (sbw-dark-powerline-two             `((t (:foreground "gray100" :background "gray20"))))
       (sbw-dark-powerline-three           `((t (:foreground "gray100" :background "gray15"))))

       (sbw-dark-muted-normal              `((t (:inherit default               :foreground ,*normal-fg*   :background ,*normal-bg*))))
       (sbw-dark-muted-selection           `((t (:inherit sbw-dark-muted-normal                            :background ,*selection-bg*))))
       (sbw-dark-muted-comment             `((t (:inherit sbw-dark-muted-normal :foreground ,*item-1*      :background ,*normal-bg*))))
       (sbw-dark-muted-string              `((t (:inherit sbw-dark-muted-normal :foreground ,*item-4*      :background ,*normal-bg*))))
       (sbw-dark-muted-emphasis            `((t (:inherit sbw-dark-muted-normal :foreground ,*emphasis-fg* :background ,*emphasis-bg*))))    
       (sbw-dark-muted-match               `((t (:inherit sbw-dark-muted-normal :foreground ,*match-fg*    :background ,*match-bg*))))
       (sbw-dark-muted-constant            `((t (:inherit sbw-dark-muted-normal :foreground ,*item-2*      :background ,*normal-bg*))))
       (sbw-dark-muted-mismatch            `((t (:inherit sbw-dark-muted-normal :foreground ,*mismatch-fg* :background ,*mismatch-bg*))))
       (sbw-dark-muted-keyword             `((t (:inherit sbw-dark-muted-normal :foreground ,*item-3*      :background ,*normal-bg*))))
       ))
  
  (custom-theme-set-faces 'sbw-dark-muted

    `(default                             ((t (:background ,*normal-bg* :foreground ,*normal-fg*))))
    
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

    ;; eshell
    '(eshell-prompt                       ((t (:inherit sbw-dark-muted-keyword))))

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
    '(helm-match                          ((t (:inherit sbw-dark-muted-match))))
    '(helm-grep-lineno                    ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-ff-symlink                     ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-ff-invalid-symlink             ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-ff-directory                   ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-buffer-process                 ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-candidate-number               ((t (:inherit sbw-dark-muted-keyword))))
    '(helm-selection                      ((t (:inherit sbw-dark-muted-selection))))

    ;; helm-swoop
    '(helm-swoop-target-word-face         ((t (:inherit sbw-dark-muted-match))))
    '(helm-swoop-target-line-face         ((t (:inherit sbw-dark-muted-selection))))
    '(helm-swoop-target-line-block-face   ((t (:inherit sbw-dark-muted-selection))))

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
    '(org-agenda-clocking                 ((t (:inherit sbw-dark-muted-normal))))
    '(org-agenda-done                     ((t (:inherit sbw-dark-muted-normal))))
    '(org-checkbox                        ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-done                            ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-level-1                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-2                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-3                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-4                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-5                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-6                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-7                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-level-8                         ((t (:inherit sbw-dark-muted-normal))))
    '(org-priority                        ((t (:inherit sbw-dark-muted-normal))))
    '(org-todo                            ((t (:inherit sbw-dark-muted-emphasis))))
    '(org-link                            ((t (:inherit sbw-dark-muted-emphasis :underline t))))
    ))

(provide-theme 'sbw-dark-muted)
