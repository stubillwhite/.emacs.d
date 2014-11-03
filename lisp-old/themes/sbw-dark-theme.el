(unless (>= 24 emacs-major-version)
  (error "Color theme requires Emacs 24 or later."))

(deftheme sbw-dark "Emacs version of my usual color scheme.")

(defgroup sbw-dark-faces nil
  "Faces used by sbw-dark theme."
  :group 'faces)

(let (
      (*background*            "gray10")
      (*comments*              "DarkSeaGreen3")
      (*constants*             "burlywood1")
      (*highlight-bg*          "darkslategray")
      (*highlight-fg*          "gray100")
      (*keywords*              "CadetBlue2")
      (*normal*                "gray80")
      (*strings*               "burlywood1")
      (*warnings*              "burlywood1")
      (*errors*                "IndianRed3")
      (*match-fg*              "gray100")
      (*match-bg*              "darkslategray")
      (*mismatch-fg*           "gray100")
      (*mismatch-bg*           "IndianRed3")
      (*mode-line-bg*          "PaleTurquoise4")
      (*mode-line-inactive-bg* "gray30")
      (*mode-line-fg*          "gray100")
      (*selection-bg*          "gray30")
      )

  (defface sbw-dark-comment
           `((t (:background ,*background* :foreground ,*comments*)))
           "Face for comments."
           :group 'sbw-dark-faces)

  (defface sbw-dark-constant
           `((t (:background ,*background* :foreground ,*constants*)))
           "Face for constants."
           :group 'sbw-dark-faces)

  (defface sbw-dark-invisible
           `((t (:background ,*background* :foreground ,*background*)))
           "Face for invisible text."
           :group 'sbw-dark-faces)

  (defface sbw-dark-keyword
           `((t (:background ,*background* :foreground ,*keywords*)))
           "Face for keywords."
           :group 'sbw-dark-faces)

  (defface sbw-dark-normal
           `((t (:background ,*background* :foreground ,*normal*)))
           "Face for normal text."
           :group 'sbw-dark-faces)

  (defface sbw-dark-match
           `((t (:background ,*match-bg* :foreground ,*match-fg*)))
           "Face for matches."
           :group 'sbw-dark-faces)

  (defface sbw-dark-mismatch
           `((t (:background ,*mismatch-bg* :foreground ,*mismatch-fg*)))
           "Face for mismatches."
           :group 'sbw-dark-faces)

  (defface sbw-dark-string
           `((t (:background ,*background* :foreground ,*strings*)))
           "Face for strings."
           :group 'sbw-dark-faces)

  (defface sbw-dark-tag
           `((t (:background ,*background* :foreground ,*highlight-fg* :weight normal)))
           "Face for tags."
           :group 'sbw-dark-faces)

  (defface sbw-dark-success
           `((t (:background ,*background* :foreground ,*comments*)))
           "Face for success."
           :group 'sbw-dark-faces)

  (defface sbw-dark-error
           `((t (:background ,*background* :foreground ,*errors*)))
           "Face for errors."
           :group 'sbw-dark-faces)

  (defface sbw-dark-warning
           `((t (:background ,*background* :foreground ,*warnings*)))
           "Face for warnings."
           :group 'sbw-dark-faces)

  (defface sbw-dark-mode-line-inactive
           `((t (:background ,*mode-line-inactive-bg* :foreground ,*mode-line-fg*)))
           "Face for active mode line."
           :group 'sbw-dark-faces)

  (defface sbw-dark-mode-line
           `((t (:background ,*mode-line-bg* :foreground ,*mode-line-fg*)))
           "Face for inactive mode line."
           :group 'sbw-dark-faces)

  (custom-theme-set-faces
    'sbw-dark

    `(bold           ((t   (:bold       t))))
    `(button         ((t   (:foreground ,*keywords*             :underline  t))))
    `(default        ((t   (:background ,*background*           :foreground ,*normal*))))
    `(header-line    ((t   (:background ,*mode-line-bg*         :foreground ,*normal*))))
    `(highlight      ((t   (:background ,*highlight-bg*         :foreground ,*highlight-fg*))))
    `(highlight-face ((t   (:background ,*highlight-bg*         :foreground ,*highlight-fg*))))
    `(info-xref      ((t   (:foreground ,*keywords*             :underline  t))))
    `(region         ((t   (:background ,*selection-bg*))))
    `(underline      ((nil (:underline  t))))

    ;; font-lock
    `(font-lock-builtin-face              ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(font-lock-comment-delimiter-face    ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-comment)))))
    `(font-lock-comment-face              ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-comment)))))
    `(font-lock-constant-face             ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-constant)))))
    `(font-lock-doc-face                  ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-string)))))
    `(font-lock-doc-string-face           ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-string)))))
    `(font-lock-function-name-face        ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(font-lock-keyword-face              ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(font-lock-negation-char-face        ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-error)))))
    `(font-lock-number-face               ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-constant)))))
    `(font-lock-preprocessor-face         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(font-lock-reference-face            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-constant)))))
    `(font-lock-regexp-grouping-backslash ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-match)))))
    `(font-lock-regexp-grouping-construct ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-match)))))
    `(font-lock-string-face               ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-string)))))
    `(font-lock-type-face                 ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(font-lock-variable-name-face        ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(font-lock-warning-face              ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-error)))))

    ;; org-mode
    `(org-agenda-clocking                 ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-agenda-done                     ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-success)))))
    `(org-checkbox                        ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-date                            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    '(org-done                            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-tag)))))
    `(org-hide                            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-invisible)))))
    `(org-level-1                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-2                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-2                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-3                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-3                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-4                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-5                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-6                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-7                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-8                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-level-9                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-link                            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword) :underline t))))
    `(org-mode-line-clock-overrun         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-tag)))))
    `(org-priority                        ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-scheduled-previously            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-error)))))
    `(org-scheduled-today                 ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-scheduled                       ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(org-special-keyword                 ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-tag)))))
    `(org-formula                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(org-tag                             ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-tag)))))
    `(org-time-grid                       ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(org-todo                            ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-tag)))))
    `(org-upcoming-deadline               ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-tag)))))
    `(org-warning                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-error)))))

    ;; Helm
    `(helm-match                          ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-match)))))
 ;   `(helm-source-header                  ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-mode-line)))))
    `(helm-grep-lineno                    ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(helm-ff-symlink                     ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(helm-ff-invalid-symlink             ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(helm-ff-directory                   ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(helm-buffer-process                 ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-keyword)))))
    `(helm-selection                      ((t (:background ,*selection-bg* ))))    ;; TODO Define a theme for this
    
    ;; Flyspell
    `(flyspell-duplicate                  ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-warning)))))
    `(flyspell-incorrect                  ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-error)))))

    ;; GUI
    `(cursor                              ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(fringe                              ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(minibuffer-prompt                   ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(mode-line-inactive                  ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-mode-line-inactive)))))
    `(mode-line                           ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-mode-line)))))
    `(text-cursor                         ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))
    `(vertical-border                     ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-normal)))))

    ;; show-paren
    `(show-paren-match                    ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-match)))))
    `(show-paren-mismatch                 ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-mismatch)))))

    ;; search
    `(isearch-fail                        ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-mismatch)))))
    `(isearch                             ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-match)))))
    `(lazy-highlight                      ((t (:background unspecified :foreground unspecified :weight normal :inherit (sbw-dark-match)))))
    ))

(provide-theme 'sbw-dark)
