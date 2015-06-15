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
_a_: Ace-Window

_q_: Quit
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
      ("g" golden-ratio-mode)
      ("=" (lambda () (interactive) (progn (golden-ratio-mode 0) (balance-windows))))
      ("q" nil :color blue)))
  
  :bind
  ("C-c w" . sbw/hydra-window/body))
      
(provide 'sbw-configure-hydra)
