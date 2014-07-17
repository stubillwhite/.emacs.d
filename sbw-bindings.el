(require 'sbw-utils)

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
    (define-key map (kbd "C-j r r") #'sbw/org-mode-redraw)
    (define-key map (kbd "C-j r s") #'sbw/org-sort-subtree)
    (define-key map (kbd "C-j v n") #'org-narrow-to-subtree)
    (define-key map (kbd "C-j v w") #'widen)
    (define-key map (kbd "C-j c l") #'org-insert-link)
    (define-key map (kbd "C-j c c") #'org-capture)

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
