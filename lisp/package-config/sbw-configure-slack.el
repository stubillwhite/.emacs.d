(require 'use-package)

(use-package slack
  :defer t

  :commands (slack-start)

  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"))
  
  :config
  (progn

    ;; Dependencies
    
    (use-package alert
      :commands (alert)
      :init
      (setq alert-default-style 'notifier))

    (use-package circe
      :init
      (progn
        (setq lui-fill-type   ""
              lui-fill-column 120)))

    (use-package emojify
      :init
      (progn
        (add-hook 'slack-mode-hook #'emojify-mode)))
    
    ;; Teams
    
    (sbw/load-secrets)
    
    (slack-register-team
     :name                "test"
     :client-id           sbw/slack-test-client-id
     :client-secret       sbw/slack-test-client-secret
     :token               sbw/slack-test-token
     :subscribed-channels '())

    (slack-register-team
     :name                "bos"
     :client-id           sbw/slack-bos-client-id
     :client-secret       sbw/slack-bos-client-secret
     :token               sbw/slack-bos-token
     :subscribed-channels '())

    ;; General configuration

    ;; Suppress user statuses
    (defun slack-user-status (_id _team) "")

    )

  (setq slack-prefer-current-team      t
        slack-display-team-name        nil
        slack-buffer-function          #'switch-to-buffer)
  
  (define-key slack-mode-map "@"
    (defun sbw/slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention))))

(provide 'sbw-configure-slack)
