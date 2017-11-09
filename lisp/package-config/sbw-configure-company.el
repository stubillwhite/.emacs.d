(require 'use-package)

(use-package company
  :diminish company-mode
  
  :init
  (progn
    (global-company-mode))

  :config
  (progn
    (setq
     company-minimum-prefix-length 2
     company-dabbrev-ignore-case   nil
     company-dabbrev-downcase      nil
     company-idle-delay            0)

    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(provide 'sbw-configure-company)
