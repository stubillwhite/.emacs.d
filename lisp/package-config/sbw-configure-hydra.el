(require 'use-package)

(use-package hydra
  :defer t

  :config
  (progn)

  :init
  (progn
    (defhydra sbw/hydra-window (:color amaranth :hint nil)
      "
^Navigate^         ^Modify^                 ^Resize^
^============================================================
_h_: Left          _V_: Vertical split      _g_: Golden ratio
_j_: Down          _H_: Horizontal split    _=_: Equal
_k_: Up            _s_: Swap
_l_: Right         _d_: Delete
_a_: Ace-Window    _b_: Buffer

_q_, _RET_: Quit
"
      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)
      ("a" ace-window)
      ("V" split-window-horizontally)
      ("H" split-window-vertically)
      ("s" (lambda () (interactive) (ace-window 4)))
      ("d" delete-window)
      ("b" helm-mini :color blue)
      ("g" golden-ratio-mode)
      ("=" (lambda () (interactive) (progn (golden-ratio-mode 0) (balance-windows))))
      ("q" nil :color blue)
      ("RET" nil : color blue))

    ;; (eval-after-load "org"
    ;;   '(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body))

    (defun org-agenda-cts ()
      (let ((args (get-text-property
                   (min (1- (point-max)) (point))
                   'org-last-args)))
        (nth 2 args)))

    (defhydra hydra-org-agenda-view (:hint nil)
      "
^Navigate^         ^Modify^                 ^Resize^
^============================================================
_d_: ?d? day       _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week      _[_: inactive      _A_: arch-files
_t_: ?t? fortnight _f_: follow=?f?    _r_: report=?r?
_m_: ?m? month     _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year      _q_: quit          _L__l__c_: ?l?
"
      ("SPC" org-agenda-reset-view)
      ("d" org-agenda-day-view  (if (eq 'day (org-agenda-cts))   "[x]" "[ ]"))
      ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts))  "[x]" "[ ]"))
      ("t" org-agenda-fortnight-view  (if (eq 'fortnight (org-agenda-cts))       "[x]" "[ ]"))
      ("m" org-agenda-month-view       (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
      ("y" org-agenda-year-view       (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
      ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
      ("L" (org-agenda-log-mode '(4)))
      ("c" (org-agenda-log-mode 'clockcheck))
      ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
      ("a" org-agenda-archives-mode)
      ("A" (org-agenda-archives-mode 'files))
      ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
      ("e" org-agenda-entry-text-mode  (format "% -3S" org-agenda-entry-text-mode))
      ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
      ("D" org-agenda-toggle-diary     (format "% -3S" org-agenda-include-diary))
      ("!" org-agenda-toggle-deadlines)
      ("[" (let ( (org-agenda-include-inactive-timestamps t) )
             (org-agenda-check-type t 'timeline 'agenda)
             (org-agenda-redo)))
      ("q" (message "Abort") :exit t))
    )
  
  :bind
  ("C-c w" . sbw/hydra-window/body))
      
(provide 'sbw-configure-hydra)


