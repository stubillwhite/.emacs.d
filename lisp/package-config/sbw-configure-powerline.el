(require 'use-package)

(use-package powerline
  :init
  (progn

    (defmacro sbw/powerline--with-powerline-faces (&rest body)
      `(lexical-let* ( (active          (powerline-selected-window-active))
                       (mode-line       (if active 'mode-line 'mode-line-inactive))
                       (face1           (if active 'sbw-dark-powerline-one-active 'sbw-dark-powerline-one-inactive))
                       (face2           'sbw-dark-powerline-two)
                       (face3           'sbw-dark-powerline-three)
                       (separator-left  (intern (format "powerline-%s-%s"
                                                  (powerline-current-separator)
                                                  (car powerline-default-separator-dir))))
                       (separator-right (intern (format "powerline-%s-%s"
                                                  (powerline-current-separator)
                                                  (cdr powerline-default-separator-dir)))) )
         ,@body))

    (defmacro sbw/powerline--with-filtered-global-mode-string (&rest body)
      `(lexical-let* ( (special-modes               (list 'sbw/countdown--mode-line-string))
                       (countdown                   (symbol-value (car (memq 'sbw/countdown--mode-line-string global-mode-string))))
                       (filtered-global-mode-string (-filter (lambda (x) (not (-contains? special-modes x))) global-mode-string)) )
         ,@body))

    (defun sbw/powerline--global-mode-string (face pad)
      (sbw/powerline--with-filtered-global-mode-string
        (powerline-raw filtered-global-mode-string face pad)))
    
    (defun sbw/powerline--countdown-timer (face pad)
      (sbw/powerline--with-filtered-global-mode-string
        (powerline-raw (when countdown (s-trim countdown)) face pad)))
    
    (defun sbw/powerline-personal-theme ()
      (setq-default mode-line-format
        '("%e"
           (:eval
             (sbw/powerline--with-powerline-faces
               (let* ( (lhs (list
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
                              (powerline-narrow face2 'l)
                              (powerline-raw " " face2)
                              ;; Level 3
                              (funcall separator-left face2 face3)                            
                              (powerline-vc face3 'r)
                              ))
                       (rhs (list
                              ;; Level 3
                              (sbw/powerline--global-mode-string face3 'r)
                              ;; Level 2
                              (funcall separator-right face3 face2)
                              (sbw/powerline--countdown-timer face2 'r)
                              ;; Level 1
                              (funcall separator-right face2 face1)
                              (powerline-raw "%4l" face1 'l)
                              (powerline-raw ":" face1 'l)
                              (powerline-raw "%3c" face1 'r)
                              (powerline-raw " " face1)
                              (powerline-raw "%6p" face1 'r)
                              )) )
                 (concat
                   (powerline-render lhs)
                   (powerline-fill face3 (powerline-width rhs))
                   (powerline-render rhs))))))))
    
    (sbw/powerline-personal-theme)
    (setq powerline-default-separator 'wave))

  :config
  (progn))

(provide 'sbw-configure-powerline)
