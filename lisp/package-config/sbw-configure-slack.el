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
     :name                "test"
     :client-id           sbw/slack-test-client-id
     :client-secret       sbw/slack-test-client-secret
     :token               sbw/slack-test-token
     :subscribed-channels '(random general))

    (slack-register-team
     :name                "elsevier-bos"
     :client-id           sbw/slack-bos-client-id
     :client-secret       sbw/slack-bos-client-secret
     :token               sbw/slack-bos-token
     :subscribed-channels '(random general))

    ;; (slack-register-team
    ;;  :name                "Elm"
    ;;  :client-id           sbw/slack-elm-client-id
    ;;  :client-secret       sbw/slack-elm-client-secret
    ;;  :token               sbw/slack-elm-token
    ;;  :subscribed-channels '())

    ;; (slack-register-team
    ;;  :name                "Clojurians"
    ;;  :client-id           sbw/slack-clojurians-client-id
    ;;  :client-secret       sbw/slack-clojurians-client-secret
    ;;  :token               sbw/slack-clojurians-token
    ;;  :subscribed-channels '()
    ;;  )

    ;; General configuration

    ;; Suppress user statuses
    (defun slack-user-status (_id _team) "")

    (setq slack-prefer-current-team      t
          slack-display-team-name        nil
          slack-buffer-function          #'switch-to-buffer)
    
    (defun sbw/slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention))

    ;; Rules

    (add-to-list 'alert-user-configuration
                 '(( (:message  . "Stu")
                     (:title    . "\\(random\\|general\\)")
                     ;; (:category . "slack")
                     )
                   nil nil))

    (defun sbw/slack-insert-newline ()
      (interactive)
      (open-line 1)
      (next-line 1)))
  
  :bind
  (:map slack-mode-map
        ("@" . sbw/slack-message-embed-mention)
        ("C-c t" . slack-thread-show-or-create)
        ("C-c r" . slack-message-add-reaction)
        ("S-<return>" . sbw/slack-insert-newline)))

(provide 'sbw-configure-slack)
;; (add-to-list 'alert-user-configuration
;;              '(( (:message  . "Etu")
;;                  ;; (:title    . "\\(random\\|general\\)")
;;                  ;; (:category . "slack")
;;                  )
;;                osx-notifier nil))

;; (defun white-test-alert (info)
;;   (string-match (plist-get info :message) "Qtu"))

;; (add-to-list 'alert-user-configuration
;;                  '(( (:message  . "Rtu")
;;                      ;; (:title    . "\\(random\\|general\\)")
;;                      ;; (:category . "slack")
;;                      )
;;                    osx-notifier nil))

;; (alert-add-rule
;;  ;; :status     '(buried visible idle)
;;  ;; :severity   '(moderate high urgent)
;;  ;; :mode       'slack-mode
;;  ;; :predicate  #'(lambda (info) (string-match (plist-get info :message) "Qtu"))
;;  :predicate #'white-test-alert
;;  :persistent #'(lambda (info) nil)
;;  ;; :style      'osx-notifier
;;  :continue   t
;;  )

;; (alert-add-rule :category "slack"
;;                 :style 'osx-notifier
;;                 :predicate (lambda (_) t)
;;                 :continue t)
