(defun sbw/countdown-remaining-time (end-time)
  "Returns a string representing the time remaining in the countdown timer."
  (format-time-string "%H:%M:%S" (time-subtract end-time (current-time))))

(defun sbw/countdown-update-timer ()
  "Update the countdown timer."
  (if (time-less-p sbw/countdown-end-time (current-time))
    (progn
      (sbw/countdown-stop))
    (progn      
      (setq sbw/countdown-mode-line-string (concat " [" (sbw/countdown-remaining-time sbw/countdown-end-time) "]"))
      (force-mode-line-update))))

(defun sbw/countdown-set-mode-line (value)
  "Control the display of the countdown timer in the mode line. VALUE may be :on to display it, or :off to hide it."
  (cond
    ((equal value :off)
      (setq sbw/countdown-mode-line-timer nil)
      (delq 'sbw/countdown-mode-line-string global-mode-string))

    ((equal value :on)
      (setq sbw/countdown-mode-line-timer nil)
      (when (not (memq 'sbw/countdown-mode-line-string global-mode-string))
        (setq global-mode-string (append global-mode-string '(sbw/countdown-mode-line-string))))))

  (force-mode-line-update))

(defun sbw/countdown-set-timer (value)
  "Control the countdown timer. VALUE may be :on to start the timer, or :off to stop it."
  (cond
    ((equal value :off)
      (when sbw/countdown-timer
        (cancel-timer sbw/countdown-timer)
        (setq sbw/countdown-timer nil)))

    ((equal value :on)
      (when sbw/countdown-timer
        (cancel-timer sbw/countdown-timer))
      (setq sbw/countdown-timer (run-with-timer 1 1 'sbw/countdown-update-timer)))))

(defun sbw/countdown-stop ()
  "Stops the countdown timer."
  (sbw/countdown-set-mode-line :off)
  (sbw/countdown-set-timer :off)
  (setq sbw/countdown-end-time nil)
  nil)

(defun sbw/countdown-start (seconds)
  "Starts the countdown timer with starting value SECONDS."
  (setq sbw/countdown-timer nil)
  (setq sbw/countdown-end-time (time-add (seconds-to-time seconds) (current-time)))
  (sbw/countdown-update-timer)
  (sbw/countdown-set-mode-line :on)
  (sbw/countdown-set-timer :on)
  nil)

(defun sbw/summarise (value)
  "Control a thirty second countdown summary timer. VALUE may be :start to start the timer, or :stop to stop it."
  (cond
    ((equal value :start) (sbw/countdown-start 30))
    ((equal value :stop)  (sbw/countdown-stop))))

(defun sbw/pomodoro (value)
  "Control a twenty-five minute pomodoro timer. VALUE may be :start to start the timer, or :stop to stop it."
  (cond
    ((equal value :start) (sbw/countdown-start (* 25 60)))
    ((equal value :stop)  (sbw/countdown-stop))))

;(sbw/summarise :start)
;(sbw/summarise :stop)
;(sbw/pomodoro :start)
;(sbw/pomodoro :stop)

;(print global-mode-string)
;(print sbw/countdown-mode-line-string)

(provide 'sbw-countdown)
