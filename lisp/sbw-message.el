;; Functions to enable and disable timestamps in the message buffer.

(defun sbw/message-enable-timestamps ()
  "Enables timestamps in the message buffer."
  (if (not (fboundp 'sbw/message--original))
    (defalias 'sbw/message--original (symbol-function 'message)))
  (defun message (fmt-string &rest args)
    (apply 'sbw/message--original
      (concat
        (format-time-string "[%H:%M:%S.%3N] " (time-subtract (current-time) sbw/emacs-start-time))
        fmt-string) args)))

(defun sbw/message-disable-timestamps ()
  "Disables timestamps in the message buffer."
  (if (fboundp 'sbw/message--original)
    (defalias 'message (symbol-function 'sbw/message--original))))

(provide 'sbw-message)
