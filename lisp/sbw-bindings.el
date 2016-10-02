
(require 'sbw-utils)

;; -----------------------------------------------------------------------------
;; New system, migrating piecemeal
;; -----------------------------------------------------------------------------

(bind-key* "<f12>"    (lambda () (interactive) (sbw/menu-display sbw/menu-common-commands)))

;; IntelliJ style bindings
(bind-key* "M-l"      'goto-line)
(bind-key* "M-f"      'isearch-forward)

(when (sbw/is-darwin?)
  ;; HOME and END should behave sensibly
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>")  'end-of-line)

  ;; fn as H-, ctrl as C-, alt as M-, cmd as s-
  (setq  
   mac-function-modifier       'hyper
   mac-control-modifier        'control
   mac-command-modifier        'meta
   mac-option-modifier         'meta
   mac-right-control-modifier  'control
   mac-right-command-modifier  'meta
   mac-right-option-modifier   'meta)
  
  ;; (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
  ;; (define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))
  )

(key-chord-define-global
 "§z"
 (defhydra hydra-zoom ()
   "zoom"
   ("=" zoom-frm-in "in")
   ("-" zoom-frm-out "out")
   ("0" zoom-frm-unzoom "reset")
   ("q" nil "quit" :color blue)))

(key-chord-define-global
 "§o"
 (defhydra hydra-org (:color red :columns 3)
   "Org Mode Movements"
   ("n" outline-next-visible-heading "next heading")
   ("p" outline-previous-visible-heading "prev heading")
   ("N" org-forward-heading-same-level "next heading at same level")
   ("P" org-backward-heading-same-level "prev heading at same level")
   ("u" outline-up-heading "up heading")
   ("g" org-goto "goto" :exit t)))

(key-chord-define-global
 "§c"
 (defhydra sbw/hydra-multiple-cursors (:color amaranth :hint nil)
   "
^Previous^         ^Next^           ^Other^
-------------------------------------------------
_p_   Previous     _n_   Next       _a_ Mark all 
_P_   Skip         _N_   Skip       _r_ Mark by regex
_M-p_ Unmark       _M-n_ Unmark
_q_   Quit
"
   ("p" mc/mark-previous-like-this)
   ("P" mc/skip-to-previous-like-this)
   ("M-p" mc/unmark-previous-like-this)
   ("n" mc/mark-next-like-this)
   ("N" mc/skip-to-next-like-this)
   ("M-n" mc/unmark-next-like-this)
   ("a" mc/mark-all-like-this :exit t)
   ("r" mc/mark-all-in-region-regexp :exit t)
   ("q" nil)))

;; -----------------------------------------------------------------------------
;; Old system
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Common
;; -----------------------------------------------------------------------------

(defun sbw/ensure-mode-has-precedence (mode)
  "Ensure that mode has precedence over other modes."
  (sbw/assq-ensure-is-first mode minor-mode-map-alist))

(defgroup sbw-modes nil
  "My custom modes.")

;; -----------------------------------------------------------------------------
;; Global key bindings
;; -----------------------------------------------------------------------------

(defvar sbw-global-mode-map (make-keymap) "sbw-global-mode keymap.")

(define-minor-mode sbw-global-mode
  "A minor mode to give my key binding precedence."
  t nil 'sbw-global-mode-map)

(sbw-global-mode 1)

;; Ensure that bindings are done last
(defadvice load (after sbw-keys-mode-has-precedence)
  "Ensure that my keybindings have precedence."
  (sbw/ensure-mode-has-precedence 'sbw-global-mode))

(ad-activate 'load)

;; Undo
(define-key sbw-global-mode-map (kbd "C-j u") 'undo-tree-visualize)

;; -----------------------------------------------------------------------------
;; Cider mode bindings
;; -----------------------------------------------------------------------------

(defvar sbw/cider-minor-mode-keymap
  (let ((map (make-sparse-keymap))) 
    (define-key map (kbd "C-j r") #'sbw/cider-reset-repl)
    (define-key map (kbd "C-j R") #'sbw/cider-refresh-repl)
    map) 
  "Keymap used when sbw/cider-minor-mode is active.")

(define-minor-mode sbw/cider-minor-mode
  "Custom minor mode for cider."
  :group   'sbw-modes
  ;:lighter " [sbw-c]"
  :keymap  sbw/cider-minor-mode-keymap
  (if sbw/cider-minor-mode
    (sbw/ensure-mode-has-precedence 'sbw/cider-minor-mode-keymap)))

(dolist (hook '(cider-mode-hook))
      (add-hook hook #'sbw/cider-minor-mode))


;; -----------------------------------------------------------------------------
;; Flyspell mode bindings
;; -----------------------------------------------------------------------------

(defun sbw/flyspell-check-next-error ()
  "Spellcheck next error in buffer."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defvar sbw-flyspell-minor-mode-keymap
  (let ((map (make-sparse-keymap))) 
    (define-key map (kbd "C-j f b") #'flyspell-buffer)
    (define-key map (kbd "C-j f n") #'sbw/flyspell-check-next-error)
    (define-key map (kbd "C-j f w") #'ispell-word)
    map) 
  "Keymap used when sbw-flyspell-minor-mode is active.")

(define-minor-mode sbw-flyspell-minor-mode
  "Custom minor mode for flyspell."
  :group   'sbw-modes
  ;:lighter " [sbw-f]"
  :keymap  sbw-flyspell-minor-mode-keymap
  (if sbw-flyspell-minor-mode
    (sbw/ensure-mode-has-precedence 'sbw-flyspell-minor-mode-keymap)))

;; TODO: There is duplication here between sbw-setup-flyspell and the key
;; bindings. Join them up.
(dolist (hook '(text-mode-hook))
      (add-hook hook #'sbw-flyspell-minor-mode))

;; -----------------------------------------------------------------------------
;; org-mode mode bindings
;; -----------------------------------------------------------------------------

(defvar sbw-org-mode-minor-mode-keymap
  (let ((map (make-sparse-keymap))) 
    (define-key map (kbd "C-j a")   #'org-agenda)
    (define-key map (kbd "C-j s m") #'org-set-tags-command)
    (define-key map (kbd "C-j s p") #'org-priority)
    (define-key map (kbd "C-j s e") #'org-set-effort)
    (define-key map (kbd "C-j s s") #'org-schedule)
    (define-key map (kbd "C-j s d") #'org-deadline)
    (define-key map (kbd "C-j t")   #'org-todo)
    (define-key map (kbd "C-j i")   #'org-clock-in)
    (define-key map (kbd "C-j n")   #'(lambda () (interactive) (org-insert-drawer "BEEF")))
    (define-key map (kbd "C-j o")   #'org-clock-out)
    (define-key map (kbd "C-j g")   #'org-clock-goto)
    (define-key map (kbd "C-j r t") #'sbw/right-align-tags)
    (define-key map (kbd "C-j r r") #'sbw/org-mode-reformat)
    (define-key map (kbd "C-j r s") #'sbw/org-sort-subtree)
    (define-key map (kbd "C-j v n") #'org-narrow-to-subtree)
    (define-key map (kbd "C-j v w") #'widen)
    (define-key map (kbd "C-j c l") #'org-insert-link)
    (define-key map (kbd "C-j c c") #'org-capture)
    (define-key map (kbd "S-<f12>") #'sbw/summarise-timer-toggle)
    (define-key map (kbd "<f12>")   #'sbw/pomodoro-timer-toggle)
    (define-key map (kbd "C-j m")   #'(lambda () (interactive) (sbw/menu-display sbw/menu-common-commands)))
    map) 
  "Keymap used when sbw-org-mode-minor-mode is active.")

;; Make windmove work in org-mode
;; TODO Fix this up
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(define-minor-mode sbw-org-mode-minor-mode
  "Custom minor mode for org-mode."
  :group   'sbw-modes
  ;:lighter " [sbw-o]"
  :keymap  sbw-org-mode-minor-mode-keymap
  (if sbw-org-mode-minor-mode
    (sbw/ensure-mode-has-precedence 'sbw-org-mode-minor-mode-keymap)))

(add-hook 'org-mode-hook #'sbw-org-mode-minor-mode)

(provide 'sbw-bindings)
