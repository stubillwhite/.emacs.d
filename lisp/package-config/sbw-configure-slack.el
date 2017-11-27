(require 'use-package)

(use-package slack
  :defer t

  :commands (slack-start)

  :init
  (progn)
  
  :config
  (progn

    ;; Dependencies
    
    (use-package alert
      :commands (alert)
      :init
      (setq alert-default-style (if (sbw/is-darwin?) 'osx-notifier 'notifier)))
   
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
     :name                "elsevier-bos"
     :client-id           sbw/slack-bos-client-id
     :client-secret       sbw/slack-bos-client-secret
     :token               sbw/slack-bos-token
     :subscribed-channels '(random general bos))

    ;; (slack-register-team
    ;;  :name                "functional-programming"
    ;;  :client-id           sbw/slack-fp-client-id
    ;;  :client-secret       sbw/slack-fp-client-secret
    ;;  :token               sbw/slack-fp-token
    ;;  :subscribed-channels '(random general))
    
    ;; General configuration

    ;; Suppress user statuses
    (defun slack-user-status (_id _team) "")

    (setq slack-prefer-current-team      t
          slack-buffer-function          #'switch-to-buffer)
    
    (defun sbw/slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention))

    ;; Rules

    ;; Ignore everything by default
    (add-to-list 'alert-user-configuration
                 '(((:category . "slack")) ignore nil))

    ;; Alert me if my name or username is mentioned in a room
    (add-to-list 'alert-user-configuration
                 '(((:message  . "\\(Stu\\|stuw\\)")
                    (:title    . "\\(random\\|general\\|bos\\)")
                    (:category . "slack"))
                   osx-notifier nil))

    ;; Keys

    (defun sbw/slack-insert-newline ()
      (interactive)
      (open-line 1)
      (next-line 1))

    ;; Temporary workaround while debugging issue #249

    (defun sbw/slack-mode--catch-message-to-string-error (orig-fun &rest args)
      (condition-case nil
          (apply orig-fun args)
        (error "<error parsing message>\n")))
    (advice-add 'slack-message-to-string :around #'sbw/slack-mode--catch-message-to-string-error)
    )
  
  :bind
  (:map slack-mode-map
        ("@" . sbw/slack-message-embed-mention)
        ("C-c t" . slack-thread-show-or-create)
        ("C-c r" . slack-message-add-reaction)
        ("C-c d" . slack-message-delete)
        ("C-c e" . slack-message-edit)
        ("S-<return>" . sbw/slack-insert-newline)))

(provide 'sbw-configure-slack)


