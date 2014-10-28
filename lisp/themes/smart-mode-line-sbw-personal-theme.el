(deftheme smart-mode-line-sbw-personal "Personal theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-sbw-personal
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil))) 
 '(mode-line-inactive ((t :foreground "gray100" :background "gray30" :inverse-video nil)))
 '(mode-line     ((t :foreground "gray100" :background "darkslategray" :inverse-video nil)))
 '(sml/global    ((t :foreground "gray80" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "gray100")))
 '(sml/filename  ((t :inherit sml/global :foreground "burlywood1")))
 '(sml/prefix    ((t :inherit sml/global :foreground "CadetBlue2")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DarkSeaGreen3")))
 '(persp-selected-face ((t :foreground "CadetBlue2" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground "IndianRed3" :background nil :inherit sml/filename))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-sbw-personal)
