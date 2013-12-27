(unless (>= 24 emacs-major-version)
  (error "Color theme requires Emacs 24 or later."))

(deftheme sbw-dark "Emacs version of my usual color scheme.")

(defgroup sbw-dark-faces nil
  "Faces used by sbw-dark theme."
  :group 'faces)

(let (
      (*background*         "gray10")
      (*comments*           "DarkSeaGreen3")
      (*constants*          "burlywood1")
      (*highlight-bg*       "darkslategray")
      (*highlight-fg*       "gray100")
      (*keywords*           "CadetBlue2")
      (*normal*             "gray80")
      (*strings*            "burlywood1")
      (*warnings*           "IndianRed3")
      (*match-fg*           "gray100")
      (*match-bg*           "darkslategray")
      (*mismatch-fg*        "gray100")
      (*mismatch-bg*        "IndianRed3")

      (*current-line*       "#1b1e2b")
      (*mode-line-bg*       "#555")
      (*mode-inactive-bg*   "#999")
      (*mode-line-fg*       "#EEE")
      (*selection*          "#CCC")
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
           `((t (:background ,*background* :foreground ,*highlight-fg*)))
           "Face for tags."
           :group 'sbw-dark-faces)

  (defface sbw-dark-warning
           `((t (:background ,*background* :foreground ,*warnings*)))
           "Face for warnings."
           :group 'sbw-dark-faces)

  (custom-theme-set-faces
    'sbw-dark

    `(bold           ((t   (:bold       t))))
    `(button         ((t   (:foreground ,*keywords*             :underline  t))))
    `(default        ((t   (:background ,*background*           :foreground ,*normal*))))
    `(header-line    ((t   (:background ,*mode-line-bg*         :foreground ,*normal*))))
    `(highlight      ((t   (:background ,*highlight-bg*         :foreground ,*highlight-fg*))))
    `(highlight-face ((t   (:background ,*highlight-bg*         :foreground ,*highlight-fg*))))
    `(hl-line        ((t   (:background ,*current-line*         :underline  t))))
    `(info-xref      ((t   (:foreground ,*keywords*             :underline  t))))
    `(region         ((t   (:background ,*selection*))))
    `(underline      ((nil (:underline  t))))

    ;; font-lock
    `(font-lock-builtin-face              ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(font-lock-comment-delimiter-face    ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-comment)))))
    `(font-lock-comment-face              ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-comment)))))
    `(font-lock-constant-face             ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-constant)))))
    `(font-lock-doc-face                  ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-string)))))
    `(font-lock-doc-string-face           ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-string)))))
    `(font-lock-function-name-face        ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(font-lock-keyword-face              ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(font-lock-negation-char-face        ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-warning)))))
    `(font-lock-number-face               ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-constant)))))
    `(font-lock-preprocessor-face         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(font-lock-reference-face            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-constant)))))
    `(font-lock-regexp-grouping-backslash ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-match)))))
    `(font-lock-regexp-grouping-construct ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-match)))))
    `(font-lock-string-face               ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-string)))))
    `(font-lock-type-face                 ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(font-lock-variable-name-face        ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(font-lock-warning-face              ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-warning)))))

    ;; org-mode
    `(org-agenda-clocking                 ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-agenda-done                     ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-checkbox                        ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-date                            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    '(org-done                            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-hide                            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-invisible)))))
    `(org-level-1                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-2                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-2                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-3                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-3                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-4                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-5                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-6                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-7                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-8                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-level-9                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-link                            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword) :underline t))))
    `(org-mode-line-clock-overrun         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-scheduled-previously            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-scheduled-today                 ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-scheduled                       ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(org-special-keyword                 ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(org-formula                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(org-tag                             ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-time-grid                       ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-keyword)))))
    `(org-todo                            ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-upcoming-deadline               ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-tag)))))
    `(org-warning                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-warning)))))

    ;; GUI
    `(cursor                              ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(fringe                              ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(minibuffer-prompt                   ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(mode-line-inactive                  ((t (:background ,*mode-inactive-bg*      :foreground ,*background*))))
    `(mode-line                           ((t (:background ,*mode-line-bg*          :foreground ,*mode-line-fg*))))
    `(text-cursor                         ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))
    `(vertical-border                     ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-normal)))))

    ;; show-paren
    `(show-paren-match                    ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-match)))))
    `(show-paren-mismatch                 ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-mismatch)))))

    ;; search
    `(isearch-fail                        ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-mismatch)))))
    `(isearch                             ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-match)))))
    `(lazy-highlight                      ((t (:background unspecified :foreground unspecified :inherit (sbw-dark-match)))))
    ))

(provide-theme 'sbw-dark)
