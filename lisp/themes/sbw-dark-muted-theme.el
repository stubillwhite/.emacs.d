(deftheme sbw-dark-muted "Dark muted theme.")

(defgroup sbw-dark-muted-faces nil
  "Faces used by sbw-dark-muted theme."
  :group 'faces)

(defun sbw/theme--create-face (name spec)
  (eval `(defface ,name ,spec "Face ,name." :group 'sbw-dark-muted-faces)))

(defun sbw/theme--inherit-from-face (name base-name)
  `(,name ((t (:inherit ,base-name)))))

(let* ( (*normal-bg*     "gray10")
        (*normal-fg*     "gray80")
        (*emphasis-bg*   "darkslategray")
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
        (*item-4*        "burlywood1")

        )

  (mapc
    (lambda (x) (apply 'sbw/theme--create-face x))
    `( (sbw-dark-powerline-one-active      `((t (:foreground "gray100" :background "SkyBlue4"))))
       (sbw-dark-powerline-one-inactive    `((t (:foreground "gray100" :background "gray30"))))
       (sbw-dark-powerline-one-evil-insert `((t (:foreground "gray100" :background "coral3"))))
       (sbw-dark-powerline-one-evil-normal `((t (:foreground "gray100" :background "coral4"))))
       (sbw-dark-powerline-two             `((t (:foreground "gray100" :background "gray20"))))
       (sbw-dark-powerline-three           `((t (:foreground "gray100" :background "gray15"))))

       (sbw-dark-muted-normal              `((t (:foreground ,*normal-fg*   :background ,*normal-bg*))))
       (sbw-dark-muted-comment             `((t (:foreground ,*item-1*      :background ,*normal-bg*))))
       (sbw-dark-muted-string              `((t (:foreground ,*item-4*      :background ,*normal-bg*))))
       (sbw-dark-muted-emphasis            `((t (:foreground ,*emphasis-fg* :background ,*emphasis-bg*))))    
       (sbw-dark-muted-match               `((t (:foreground ,*match-fg*    :background ,*match-bg*))))
       (sbw-dark-muted-constant            `((t (:foreground ,*item-2*      :background ,*normal-bg*))))
       (sbw-dark-muted-mismatch            `((t (:foreground ,*mismatch-fg* :background ,*mismatch-bg*))))
       


       
       ))

  

  


  
  (custom-theme-set-faces
    'sbw-dark-muted

    `(default                ((t (:background ,*normal-bg* :foreground ,*normal-fg*))))
    
    `(bold                   ((t (:inherit sbw-dark-muted-normal   :bold t))))
    `(underline              ((t (:inherit sbw-dark-muted-normal   :underline t))))
    `(highlight              ((t (:inherit sbw-dark-muted-emphasis))))
    `(lazy-highlight         ((t (:inherit sbw-dark-muted-emphasis))))

    `(isearch                ((t (:inherit sbw-dark-muted-match))))
    `(isearch-fail           ((t (:inherit sbw-dark-muted-mismatch))))

    `(show-paren-match       ((t (:inherit sbw-dark-muted-match))))
    `(show-paren-mismatch    ((t (:inherit sbw-dark-muted-mismatch))))

    `(font-lock-comment-face           ((t (:inherit sbw-dark-muted-comment))))
    `(font-lock-comment-delimiter-face ((t (:inherit sbw-dark-muted-comment))))
    `(font-lock-string-face            ((t (:inherit sbw-dark-muted-string))))
    ))

 

(provide-theme 'sbw-dark-muted)
