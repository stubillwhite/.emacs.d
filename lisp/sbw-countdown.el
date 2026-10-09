;;; sbw-countdown.el --- Countdown timers -*- lexical-binding: t; -*-

(defvar sbw/countdown--state
  (list
   :timer     nil
   :display   nil
   :end-time  nil
   :on-expire nil)
  "The state of the countdown timer.")

(defvar sbw/countdown--mode-line-string ""
  "Mode line text showing the countdown remaining.")

(defmacro sbw/countdown--with-state (state &rest body)
  `(let* ((timer    (plist-get ,state :timer))
          (display  (plist-get ,state :display))
          (end-time (plist-get ,state :end-time)))
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
        (sbw/countdown--expire)
      (setq sbw/countdown--mode-line-string
            (format " [%s]" (sbw/countdown-remaining-as-string)))
      (force-mode-line-update))))

(defun sbw/countdown--expire ()
  (let ((on-expire (plist-get sbw/countdown--state :on-expire)))
    (sbw/countdown--clear-timer)
    (if on-expire
        (funcall on-expire)
      (message "Timer expired.")
      (beep t))))

(defun sbw/countdown--set-display (value)
  (if (equal value :on)
      (unless (memq 'sbw/countdown--mode-line-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(sbw/countdown--mode-line-string))))
    (setq global-mode-string
          (delq 'sbw/countdown--mode-line-string global-mode-string)))
  (force-mode-line-update))

(defun sbw/countdown--set-timer (value)
  (sbw/countdown--with-state sbw/countdown--state
    (when timer (cancel-timer timer))
    (plist-put sbw/countdown--state :timer
               (when (equal value :on)
                 (run-with-timer 1 1 #'sbw/countdown--update-timer)))))

(defun sbw/countdown--set-end-time (time)
  (plist-put sbw/countdown--state :end-time time))

(defun sbw/countdown--clear-timer ()
  (sbw/countdown--set-display :off)
  (sbw/countdown--set-timer :off)
  (sbw/countdown--set-end-time nil)
  (plist-put sbw/countdown--state :on-expire nil))

(defun sbw/countdown-stop ()
  "Stops the countdown timer."
  (sbw/countdown--clear-timer)
  (message "Timer stopped")
  nil)

(defun sbw/countdown-start (seconds &optional on-expire)
  "Starts the countdown timer with starting value SECONDS. When it expires, call ON-EXPIRE (if given) instead of just beeping."
  (sbw/countdown--set-timer :off)
  (plist-put sbw/countdown--state :on-expire on-expire)
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

(defvar sbw/stretch-seconds 6
  "Length in seconds of each stretch and each relax period.")

(defvar sbw/stretch-cycles 12
  "Number of stretch/relax cycles per session.")

(defun sbw/stretch--run (cycle phase)
  "Run PHASE (:stretch or :relax) of CYCLE, then chain to the next phase."
  (if (> cycle sbw/stretch-cycles)
      (progn (message "Stretching complete")
             (beep t))
    (sbw/countdown-start
     sbw/stretch-seconds
     (lambda ()
       (if (eq phase :stretch)
           (sbw/stretch--run cycle :relax)
         (sbw/stretch--run (1+ cycle) :stretch))))
    (message "%s (%d/%d)"
             (if (eq phase :stretch) "Stretch" "Relax")
             cycle sbw/stretch-cycles)
    (beep t)))

(defun sbw/stretch-toggle ()
  "Toggle neck stretch routine."
  (interactive)
  (if (sbw/countdown-running?)
      (sbw/countdown-stop)
    (sbw/stretch--run 1 :stretch)))

(provide 'sbw-countdown)
