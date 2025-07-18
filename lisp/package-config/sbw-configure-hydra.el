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
_h_: Left          _|_: Vertical split      _g_: Golden ratio
_j_: Down          _-_: Horizontal split    _=_: Equal
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
      ("-" split-window-vertically)
      ("|" split-window-horizontally)
      ("s" (lambda () (interactive) (ace-window 4)))
      ("d" delete-window)
      ("b" helm-mini :color blue)
      ("g" golden-ratio-mode)
      ("=" (lambda () (interactive) (progn (golden-ratio-mode 0) (balance-windows))))
      ("q" nil :color blue)
      ("RET" nil :color blue))

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

    (defun sbw/configure-hydra-org-clear-tags-and-matrix ()
      (let* ((tags '(catchup today thisWeek nextWeek admin)))
        (-map (lambda (x) (org-agenda-set-tags (symbol-name x) 'off)) tags)
        (sbw/org-delete-property "MATRIX")
        (message "Cleared tags and matrix")))
    
    (defun sbw/configure-hydra-org-set-timescale-tag (tag)
      (sbw/configure-hydra-org-clear-tags-and-matrix)
      (org-agenda-set-tags tag 'on)
      (message (concat "Tagged '" tag "'")))

    (defun sbw/configure-hydra-org-set-matrix (matrix)
      (sbw/configure-hydra-org-clear-tags-and-matrix)
      (sbw/org-set-property "MATRIX" matrix)
      (message (concat "Matrix set to '" matrix "'")))
    
    (defhydra hydra-org-agenda-retag-view (:color :amaranth :hint nil)
      "
^Navigate^           ^Schedule^        ^Matrix^
^============================================================
_C-n_: next item     _d_: today        _1_: do it
_C-p_: prev item     _w_: this week    _2_: schedule it
^^                   _n_: next week    _3_: delegate it
^^                   _a_: admin        _4_: de-clutter it

_c_: clear
_r_: refresh
_q_: quit 
"
      ("C-n" (lambda () (interactive) (org-agenda-next-item 1)))
      ("C-p" (lambda () (interactive) (org-agenda-previous-item 1)))
      ("d" (lambda () (interactive) (sbw/configure-hydra-org-set-timescale-tag "today")))
      ("w" (lambda () (interactive) (sbw/configure-hydra-org-set-timescale-tag "thisWeek")))
      ("n" (lambda () (interactive) (sbw/configure-hydra-org-set-timescale-tag "nextWeek")))
      ("a" (lambda () (interactive) (sbw/configure-hydra-org-set-timescale-tag "admin")))
      ("1" (lambda () (interactive) (sbw/configure-hydra-org-set-matrix "urgent-important")))
      ("2" (lambda () (interactive) (sbw/configure-hydra-org-set-matrix "not-urgent-important")))
      ("3" (lambda () (interactive) (sbw/configure-hydra-org-set-matrix "urgent-not-important")))
      ("4" (lambda () (interactive) (sbw/configure-hydra-org-set-matrix "not-urgent-not-important")))
      ("c" (lambda () (interactive) (sbw/configure-hydra-org-clear-tags-and-matrix)))
      ("r" (lambda () (interactive) (org-agenda-redo)))
      ("q" (message "Done") :exit t))
    
    (defhydra hydra-org-set-matrix (:color :amaranth :hint nil)
      "
^Urgency^
^==============
_u_: Urgent        (do or delegate)
_n_: Not urgent    (schedule or de-clutter)
_d_: Delete

_q_, _RET_: Quit
"
      ("u" (lambda () (interactive) (sbw/org-set-property "MATRIX" "urgent")     (hydra-org-set-matrix-2/body)) :color blue)
      ("n" (lambda () (interactive) (sbw/org-set-property "MATRIX" "not-urgent") (hydra-org-set-matrix-2/body)) :color blue)
      ("d" (lambda () (interactive) (sbw/org-delete-property "MATRIX")) :color blue)
      ("q" nil :color blue)
      ("RET" nil :color blue))

    (defhydra hydra-org-set-matrix-2 (:color :amaranth :hint nil)
      "
^Importance^
^=================
_i_: Important     (do or schedule)
_n_: Not important (delegate or de-clutter)
_d_: Delete

_q_, _RET_: Quit
"
      ("i" (lambda () (interactive) (sbw/org-set-property "MATRIX" (concat (sbw/org-get-property "MATRIX") "-important"))) :color blue)
      ("n" (lambda () (interactive) (sbw/org-set-property "MATRIX" (concat (sbw/org-get-property "MATRIX") "-not-important"))) :color blue)
      ("d" (lambda () (interactive) (sbw/org-delete-property "MATRIX")) :color blue)
      ("q" nil :color blue)
      ("RET" nil :color blue))
    ))
      
(provide 'sbw-configure-hydra)
