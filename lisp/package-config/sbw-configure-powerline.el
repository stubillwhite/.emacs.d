(require 'use-package)

(use-package powerline
  :init
  (progn

    (defun sbw/powerline-personal-theme ()
      (setq-default mode-line-format
        '("%e"
           (:eval
             (let* ( (active          (powerline-selected-window-active))
                     (mode-line       (if active 'mode-line 'mode-line-inactive))
                     (face1           (if active 'sbw-dark-powerline-one-active 'sbw-dark-powerline-one-inactive))
                     (face2           'sbw-dark-powerline-two)
                     (face3           'sbw-dark-powerline-three)
                     (separator-left  (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (car powerline-default-separator-dir))))
                     (separator-right (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (cdr powerline-default-separator-dir))))
                     (lhs (list
                            (powerline-raw "%*" face1)
                            (powerline-raw " " face1)
                            (powerline-buffer-size face1)
                            (powerline-raw " " face1)
                            (powerline-raw mode-line-mule-info face1)
                            (powerline-buffer-id face1)
                            (powerline-raw " " face1)
                            (funcall separator-left face1 face2)
                            (powerline-major-mode face2 'l)
                            (powerline-process face2)
                            (powerline-minor-modes face2 'l)
                            (powerline-narrow face1 'l)
                            (powerline-raw " " face2)
                            (funcall separator-left face2 face3)                            
                            (powerline-raw " WHITE " face3)
                            (funcall separator-left face3 face2)
                            (powerline-vc face2 'r)
                            ))
                     (rhs (list
                            (powerline-raw global-mode-string face2 'r)
                            (funcall separator-right face2 face1)
                            (powerline-raw "%4l" face1 'l)
                            (powerline-raw ":" face1 'l)
                            (powerline-raw "%3c" face1 'r)
                            (funcall separator-right face1 mode-line)
                            (powerline-raw " " mode-line)
                            (powerline-raw "%6p" nil 'r)
                            )))
               (concat
                 (powerline-render lhs)
                 (powerline-fill face2 (powerline-width rhs))
                 (powerline-render rhs)))))))
    
    ;;(powerline-default-theme)
    (sbw/powerline-personal-theme)
    (setq powerline-default-separator 'wave))

  :config
  (progn))

(provide 'sbw-configure-powerline)
