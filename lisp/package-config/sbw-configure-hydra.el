(require 'use-package)

(use-package hydra
  :defer t

  :config
  (progn)

  :init
  (progn
    (defhydra sbw/hydra-window (:color amaranth :hint nil)
      "
^Navigate^       ^Modify^
^-------------------------------------
_h_: Left        _V_: Vertical split
_j_: Down        _H_: Horizontal split
_k_: Up          _s_: Swap
_l_: Right       _d_: Delete
_a_: Ace-Window
_q_: Exit
"
      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)
      ("a" ace-window)
      ("V" split-window-horizontally)
      ("H" split-window-vertically)
      ("s" (lambda () (interactive) (ace-window 4)))
      ("d" (lambda () (interactive) (ace-window 16)))
      ("q" (lambda () (interactive) nil) :color blue)))
  
  :bind
  ("<f2>" . sbw/hydra-window/body))
      
(provide 'sbw-configure-hydra)
