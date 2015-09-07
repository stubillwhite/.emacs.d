;; Customise how messages are displayed in the *Messages* buffer.

(require 'cl)

(defun sbw/message-wrap-with (f)
  "Wrap calls to message with F, where F is a function taking a format string and optional arguments."
  (lexical-let ( (f f) )
    (if (not (fboundp 'sbw/message--original))
      (defalias 'sbw/message--original (symbol-function 'message)))
    (defun message (fmt-string &rest args)
      (sbw/message--original "%s" (apply f fmt-string args)))))

(defun sbw/message-unwrap ()
  "Unwrap calls to message."
  (if (fboundp 'sbw/message--original)
    (defalias 'message (symbol-function 'sbw/message--original))))

(defun sbw/message-enable-timestamps ()
  "Display the display of timestamps in the message buffer."
  (sbw/message-wrap-with
    (lambda (fmt-string &rest args)
      (apply 'format
        (concat
          (format-time-string "[%H:%M:%S.%3N] ")
          fmt-string)
        args))))

(defun sbw/message-reset ()
  "Reset message to default, removing any wrappers."
  (sbw/message-unwrap))

(defun sbw/message-enable-indent (n)
  "Display an indent of N spaces before each message."
  (lexical-let ((n n))
    (sbw/message-wrap-with
      (lambda (fmt-string &rest args)
        (apply 'format
          (concat (make-string n ?\s) fmt-string)
          args)))))

(provide 'sbw-message)

