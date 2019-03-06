(defvar sbw/countdown--state
  (list
   :timer    nil
   :display  nil
   :end-time nil)
  "The state of the countdown timer.")
  
(defmacro sbw/countdown--with-state (state &rest body)
    `(lexical-let* ( (timer    (plist-get ,state :timer))
                     (display  (plist-get ,state :display))
                     (end-time (plist-get ,state :end-time)) )
     ,@body))

(defun sbw/countdown-running? ()
  "Returns t when a countdown is running, nil otherwise."
  (sbw/countdown--with-state sbw/countdown--state
                             (when timer t)))
  
(defun sbw/countdown--remaining ()
  (sbw/countdown--with-state sbw/countdown--state
                             (time-subtract end-time (current-time))))

(defun sbw/countdown-remaining-as-time ()
  "Returns the time remaining in the countdown timer as a time."
  (sbw/countdown--remaining))

(defun sbw/countdown-remaining-as-string ()
  "Returns the time remaining in the countdown timer as a string."
  (format-time-string "%H:%M:%S" (sbw/countdown-remaining-as-time) "utc"))

(defun sbw/countdown--update-timer ()
  (sbw/countdown--with-state sbw/countdown--state
               (if (time-less-p end-time (current-time))
                   (progn
                     (sbw/countdown--expire))
                 (progn      
                   (setq sbw/countdown--mode-line-string (format " [%s]" (sbw/countdown-remaining-as-string)))
                   (force-mode-line-update)))))

(defun sbw/countdown--expire ()
  (sbw/countdown--clear-timer)
  (message "Timer expired.")
  (beep t))

(defun sbw/countdown--set-display (value)
  (if (equal value :on)
      (when (not (memq 'sbw/countdown--mode-line-string global-mode-string))
        (setq global-mode-string (append global-mode-string '(sbw/countdown--mode-line-string))))
    (delq 'sbw/countdown--mode-line-string global-mode-string))
  (force-mode-line-update))

(defun sbw/countdown--set-timer (value)
  (sbw/countdown--with-state sbw/countdown--state
               (when timer (cancel-timer timer))
               (plist-put sbw/countdown--state :timer 
                          (if (equal value :on)
                              (run-with-timer 1 1 'sbw/countdown--update-timer)
                            nil))))

(defun sbw/countdown--set-end-time (time)
  (plist-put sbw/countdown--state :end-time time))

(defun sbw/countdown--clear-timer ()
  (sbw/countdown--set-display :off)
  (sbw/countdown--set-timer :off)
  (sbw/countdown--set-end-time nil))
  
(defun sbw/countdown-stop ()
  "Stops the countdown timer."
  (sbw/countdown--clear-timer)
  (message "Timer stopped")
  nil)

(defun sbw/countdown-start (seconds)
  "Starts the countdown timer with starting value SECONDS."
  (sbw/countdown--set-timer :off)
  (sbw/countdown--set-end-time (time-add (seconds-to-time seconds) (current-time)))
  (sbw/countdown--update-timer)
  (sbw/countdown--set-display :on)
  (sbw/countdown--set-timer :on)
  (message "Timer started")
  nil)
  
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
