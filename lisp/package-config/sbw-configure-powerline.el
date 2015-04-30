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
                     (face1           (if active 'powerline-active1 'powerline-inactive1))
                     (face2           (if active 'powerline-active2 'powerline-inactive2))
                     (face3           (if active 'powerline-inactive3 'powerline-inactive2))
                     (separator-left  (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (car powerline-default-separator-dir))))
                     (separator-right (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (cdr powerline-default-separator-dir))))
                     (lhs (list
                            (powerline-raw "%*" nil 'l)
                            (powerline-buffer-size nil 'l)
                            (powerline-raw mode-line-mule-info nil 'l)
                            (powerline-buffer-id nil 'l)
                            (powerline-raw " ")
                            (funcall separator-left mode-line face1)
                            (powerline-major-mode face1 'l)
                            (powerline-process face1)
                            (powerline-minor-modes face1 'l)
                            (powerline-narrow face1 'l)
                            (powerline-raw " " face1)
                            (powerline-raw " WHITE " face3)
                            (funcall separator-left face1 face2)
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
