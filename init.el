;; Hack to override local loading

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Normal configuration

;; Configure paths
(setq sbw/lisp-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path sbw/lisp-path)
(add-to-list 'load-path (concat sbw/lisp-path "/test"))
(add-to-list 'custom-theme-load-path (concat sbw/lisp-path "/themes"))

(print sbw/lisp-path)

;; Bootstrap packges
(require 'sbw-common-config)
(require 'sbw-bootstrap)
(require 'sbw-package-list)
(require 'sbw-cosmetics)

(sbw/bootstrap-init)
(sbw/bootstrap-packages sbw/package-list)

(sbw/bootstrap-require sbw/personal-package-list)

(straight-use-package
 '(org-ai :type git :host github :repo "rksm/org-ai"
          :local-repo "org-ai"
          :files ("*.el" "README.md" "snippets")))


(use-package org-ai
  :custom
  (org-ai-openai-api-token "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")

  :init
  (add-hook 'org-mode-hook #'org-ai-mode)

  :config
  ;; if you are on the gpt-4 beta:
  ;; (setq org-ai-default-chat-model "gpt-4")
  ;; (setq org-ai-default-chat-model "chatgpt") 
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))


;; (defun sbw/org-ai--wrap-text (&rest args)
;;   (print args)
;;   (fill-paragraph)
;;   )

;; (add-hook 'org-ai-after-chat-insertion-hook #'sbw/org-ai--wrap-text)
