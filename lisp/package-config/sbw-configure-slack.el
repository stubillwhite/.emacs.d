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

    ;; (slack-register-team
    ;;  :name                "white-test"
    ;;  :client-id           sbw/slack-test-client-id
    ;;  :client-secret       sbw/slack-test-client-secret
    ;;  :token               sbw/slack-test-token
    ;;  :subscribed-channels '(random general))
    
    (slack-register-team
     :name                "elsevier-bos"
     :client-id           sbw/slack-bos-client-id
     :client-secret       sbw/slack-bos-client-secret
     :token               sbw/slack-bos-token
     :subscribed-channels '(random general bos))

    (slack-register-team
     :name                "mendeley"
     :client-id           sbw/slack-mendeley-client-id 
     :client-secret       sbw/slack-mendeley-client-secret
     :token               sbw/slack-mendeley-token
     :subscribed-channels '(random general newsflo newsflo-alerts newsflodevs bos-big-data scala engineering))
    
    ;; (slack-register-team
    ;;  :name                "functional-programming"
    ;;  :client-id           sbw/slack-fp-client-id
    ;;  :client-secret       sbw/slack-fp-clseient-secret
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

    ;; Notifications

    ;; For some rooms I want the content of attachments to be considered when processing notifications
    
    (setq sbw/slack--mettachment-notifications-rooms '("newsflo-alerts"))

    (defun sbw/slack--message-notify-alert-with-attachment (message room team)
      (with-slots (text attachments) message
        (let* ((attachment-to-string (lambda (x) (slack-message-to-string x (slack-message-image-to-string message team))))
               (attachment-strings   (mapconcat attachment-to-string attachments "\n\t-\n"))
               (attachment-text      (slack-message-unescape-string attachment-strings team)))
          (alert attachment-text
                 :title    (slack-room-name room)
                 :icon     slack-alert-icon
                 :category 'slack))))

    (defun sbw/slack--custom-notifier (message room team)
      (let* ((room-name (slack-room-name room)))
        (if (member room-name sbw/slack--attachment-notifications-rooms)
            (sbw/slack--message-notify-alert-with-attachment message room team)
          (slack-message-notify-alert message room team))))

    (setq slack-message-custom-notifier 'sbw/slack--custom-notifier)

    ;; Notification rules

    ;; Clear everything out to simplify reloading this configuration when making changes
    (setq alert-user-configuration '())
    
    ;; Ignore everything by default
    (add-to-list 'alert-user-configuration
                 '(((:category . "slack"))
                   ignore nil))

    ;; Notify me if my name or username is mentioned in any of my subscribed channels
    (add-to-list 'alert-user-configuration
                 '(((:message  . "\\(Stu\\|stuw\\)")
                    (:category . "slack"))
                   osx-notifier nil))

    ;; Notify me for anything in important channels
    (add-to-list 'alert-user-configuration
                 '(((:title    . "\\(^newsflo$\\|^newsflodevs$\\|^scala$\\)")
                    (:category . "slack"))
                   osx-notifier nil))

    ;; Notify me for warnings or failures in the alerts channels
    (add-to-list 'alert-user-configuration
                 '(((:title    . "\\(^newsflo-alerts$\\)")
                    (:message  . "\\(Failure\\|Warning\\)")
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
        (error "[Error parsing message]\n")))
    (advice-add 'slack-message-to-string :around #'sbw/slack-mode--catch-message-to-string-error)

    ;; UI

    (add-hook 'slack-mode-hook #'sbw/flyspell--enable-flyspell-mode)
    
    (defun set-window-width (n)
      (window-resize (selected-window) (- n (window-width)) t))

    (defun sbw/slack-mode--open-window ()
      (interactive)
      (let* ((new-window (split-window-right)))
        (select-window new-window)
        (set-window-width 129)
        (slack-start))))
  
  :bind
  (:map slack-mode-map
        ("@" . sbw/slack-message-embed-mention)
        ("C-c t" . slack-thread-show-or-create)
        ("C-c r" . slack-message-add-reaction)
        ("C-c d" . slack-message-delete)
        ("C-c e" . slack-message-edit)
        ("S-<return>" . sbw/slack-insert-newline)))

(provide 'sbw-configure-slack)
