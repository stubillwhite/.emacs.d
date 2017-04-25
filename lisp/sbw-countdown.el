(require 'names)

(define-namespace sbw/countdown-

  (defvar -state
    (list
      :timer    nil
      :display  nil
      :end-time nil)
    "The state of the countdown timer.")
  
  (defmacro -with-state (state &rest body)
    `(lexical-let* ( (timer    (plist-get ,state :timer))
                     (display  (plist-get ,state :display))
                     (end-time (plist-get ,state :end-time)) )
     ,@body))

  (defun running? ()
    "Returns t when a countdown is running, nil otherwise."
    (-with-state sbw/countdown--state
      (when timer t)))
  
  (defun -remaining ()
    (-with-state sbw/countdown--state
      (time-subtract end-time (current-time))))

  (defun remaining-as-time ()
    "Returns the time remaining in the countdown timer as a time."
    (-remaining))

  (defun remaining-as-string ()
    "Returns the time remaining in the countdown timer as a string."
    (format-time-string "%H:%M:%S" (remaining-as-time) "utc"))

  (defun -update-timer ()
    (-with-state sbw/countdown--state
      (if (time-less-p end-time (current-time))
        (progn
          (sbw/countdown--expire))
        (progn      
          (setq sbw/countdown--mode-line-string (format " [%s]" (sbw/countdown-remaining-as-string)))
          (force-mode-line-update)))))

  (defun -expire ()
    (-clear-timer)
    (message "Timer expired.")
    (beep t))

  (defun -set-display (value)
    (if (equal value :on)
      (when (not (memq 'sbw/countdown--mode-line-string global-mode-string))
        (setq global-mode-string (append global-mode-string '(sbw/countdown--mode-line-string))))
      (delq 'sbw/countdown--mode-line-string global-mode-string))
    (force-mode-line-update))

  (defun -set-timer (value)
    (-with-state sbw/countdown--state
      (when timer (cancel-timer timer))
      (plist-put sbw/countdown--state :timer 
        (if (equal value :on)
          (run-with-timer 1 1 'sbw/countdown--update-timer)
          nil))))

  (defun -set-end-time (time)
    (plist-put -state :end-time time))

  (defun -clear-timer ()
    (-set-display :off)
    (-set-timer :off)
    (-set-end-time nil))
  
  (defun stop ()
    "Stops the countdown timer."
    (-clear-timer)
    (message "Timer stopped")
    nil)

  (defun start (seconds)
    "Starts the countdown timer with starting value SECONDS."
    (-set-timer :off)
    (-set-end-time (time-add (seconds-to-time seconds) (current-time)))
    (-update-timer)
    (-set-display :on)
    (-set-timer :on)
    (message "Timer started")
    nil)
  )

(defun sbw/summarise-timer-toggle ()
  "Toggles a thirty second summary timer."
  (interactive)
  (if (sbw/countdown-running?)
    (sbw/countdown-stop)
    (sbw/countdown-start 30)))

(defun sbw/pomodoro-timer-toggle ()
  "Toggles a twenty-five minute pomodoro timer."
  (interactive)
  (if (sbw/countdown-running?)
    (sbw/countdown-stop)
    (sbw/countdown-start (* 25 60))))

(defun sbw/unit-timer-toggle ()
  "Toggles a forty minute work unit timer."
  (interactive)
  (if (sbw/countdown-running?)
    (sbw/countdown-stop)
    (sbw/countdown-start (* 40 60))))

(provide 'sbw-countdown)
