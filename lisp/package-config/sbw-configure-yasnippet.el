(require 'use-package)

(use-package yasnippet
  :defer t

  :init
  (progn) 

  :config
  (progn
    (setq yas-snippet-dirs "~/.emacs.d/snippets")
    ;;(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

    (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
    
    (yas-reload-all)


    
    ;;(yas-initialize)
    ;;(unbind-key "TAB")
    )
  
;  :bind
;  ("<backtab>" . yas-expand)
  )

(provide 'sbw-configure-yasnippet)

