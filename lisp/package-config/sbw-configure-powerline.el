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
                            ;; Level one
                            (powerline-raw "%*" face1)
                            (powerline-raw " " face1)
                            (powerline-buffer-size face1)
                            (powerline-raw " " face1)
                            (powerline-raw mode-line-mule-info face1)
                            (powerline-raw " " face1)
                            (powerline-buffer-id face1)
                            (powerline-raw " " face1)
                            ;; Level 2
                            (funcall separator-left face1 face2)
                            (powerline-major-mode face2 'l)
                            (powerline-process face2)
                            (powerline-minor-modes face2 'l)
                            (powerline-narrow face1 'l)
                            (powerline-raw " " face2)
                            ;; Level 3
                            (funcall separator-left face2 face3)                            
                            (powerline-vc face3 'r)
                            ))
                     (rhs (list
                            ;; Level 3
                            (powerline-raw global-mode-string face3 'r)
                            ;; Level 2
                            (funcall separator-right face3 face2)
                            (powerline-raw "%4l" face2 'l)
                            (powerline-raw ":" face2 'l)
                            (powerline-raw "%3c" face2 'r)
                            ;; Level 1
                            (funcall separator-right face2 face1)
                            (powerline-raw " " face1)
                            (powerline-raw "%6p" face1 'r)
                            )))
               (concat
                 (powerline-render lhs)
                 (powerline-fill face3 (powerline-width rhs))
                 (powerline-render rhs)))))))
    
    (sbw/powerline-personal-theme)
    (setq powerline-default-separator 'wave))

  :config
  (progn))

(provide 'sbw-configure-powerline)
