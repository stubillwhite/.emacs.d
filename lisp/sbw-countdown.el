(require 'names)

(define-namespace sbw/countdown-
  
  (defun -remaining-time (end-time)
    "Returns a string representing the time remaining in the countdown timer."
    (format-time-string "%H:%M:%S" (time-subtract end-time (current-time)) :utc))

  (defun -expired-inform-user ()
    (message "Countdown expired.")
    (beep t))

  (defun -update-timer ()
    (if (time-less-p end-time (current-time))
      (progn
        (-expired-inform-user)
        (-stop))
      (progn      
        (setq mode-line-string (concat " [" (-remaining-time end-time) "]"))
        (force-mode-line-update))))

  (defun -set-mode-line (value)
    "Control the display of the countdown timer in the mode line. VALUE may be :on to display it, or :off to hide it."
    (cond
      ((equal value :off)
        (setq mode-line-timer nil)
        (delq 'mode-line-string global-mode-string))

      ((equal value :on)
        (setq mode-line-timer nil)
        (when (not (memq 'mode-line-string global-mode-string))
          (setq global-mode-string (append global-mode-string '(mode-line-string))))))

    (force-mode-line-update))

  (defun -set-timer (value)
    "Control the countdown timer. VALUE may be :on to start the timer, or :off to stop it."
    (cond
      ((equal value :off)
        (when timer
          (cancel-timer timer)
          (setq timer nil)))

      ((equal value :on)
        (when timer
          (cancel-timer timer))
        (setq timer (run-with-timer 1 1 'sbw/countdown--update-timer)))))

  (defun -stop ()
    "Stops the countdown timer."
    (-set-mode-line :off)
    (-set-timer :off)
    (setq end-time nil)
    (message "Timer stopped")
    nil)

  (defun -start (seconds)
    "Starts the countdown timer with starting value SECONDS."
    (setq timer nil)
    (setq end-time (time-add (seconds-to-time seconds) (current-time)))
    (-update-timer)
    (-set-mode-line :on)
    (-set-timer :on)
    (message "Timer started")
    nil)

  (defun running? ()
    "Returns t when a countdown is running, nil otherwise."
    (when (bound-and-true-p timer) t))

  (defun toggle (seconds)
    "If the coundown timer is stopped then start it with duration SECONDS, otherwise stop it."
    (if (running?)
      (-stop)
      (-start seconds))))

(defun sbw/summarise-timer-toggle ()
  "Toggles a thirty second summary timer."
  (interactive)
  (sbw/countdown-toggle 30))

(defun sbw/pomodoro-timer-toggle ()
  "Toggles a twenty-five minute pomodoro timer."
  (interactive)
  (sbw/countdown-toggle (* 25 60)))

(provide 'sbw-countdown)
