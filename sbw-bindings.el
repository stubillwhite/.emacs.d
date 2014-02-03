;; -----------------------------------------------------------------------------
;; Common
;; -----------------------------------------------------------------------------

(defun sbw-bindings/ensure-is-first (key alist)
  "Mutates alist so that the value for key is first."
  (let ((entry (assq key alist)))
    (assq-delete-all key alist)
    (add-to-list 'alist entry)))

(defun sbw-bindings/ensure-mode-has-precedence (mode)
  "Ensure that mode has precedence over other modes."
  (sbw-bindings/ensure-is-first mode minor-mode-map-alist))

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
  (sbw-bindings/ensure-mode-has-precedence 'sbw-global-mode))

(ad-activate 'load)

;; undo
(define-key sbw-global-mode-map (kbd "C-j u") 'undo-tree-visualize)

;; -----------------------------------------------------------------------------
;; Flyspell mode bindings
;; -----------------------------------------------------------------------------

(defun sbw-bindings/flyspell-check-next-error ()
  "Spellcheck next error in buffer."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defvar sbw-flyspell-minor-mode-keymap
  (let ((map (make-sparse-keymap))) 
    (define-key map (kbd "C-j f b") #'flyspell-buffer)
    (define-key map (kbd "C-j f n") #'sbw-bindings/flyspell-check-next-error)
    (define-key map (kbd "C-j f w") #'ispell-word)
    map) 
  "Keymap used when sbw-flyspell-minor-mode is active.")

(define-minor-mode sbw-flyspell-minor-mode
  "Custom minor mode for flyspell."
  :group   'sbw-modes
  :keymap  sbw-flyspell-minor-mode-keymap
  (if sbw-flyspell-minor-mode
    (sbw-bindings/ensure-mode-has-precedence 'sbw-flyspell-minor-mode-keymap)))

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
    (define-key map (kbd "C-j t")   #'org-set-tags-command)
    (define-key map (kbd "C-j e")   #'org-set-effort)
    (define-key map (kbd "C-j s")   #'org-todo)
    (define-key map (kbd "C-j c")   #'org-schedule)
    (define-key map (kbd "C-j d")   #'org-deadline)
    (define-key map (kbd "C-j i")   #'org-clock-in)
    ;(define-key map (kbd "C-j n")   #'(lambda () (interactive) (org-insert-drawer)))
    (define-key map (kbd "C-j o")   #'org-clock-out)
    (define-key map (kbd "C-j g")   #'org-clock-goto)
    (define-key map (kbd "C-j r")   #'sbw-org-mode-right-align-tags)
    (define-key map (kbd "C-j u")   #'org-update-all-dblocks)
    (define-key map (kbd "C-j v n") #'org-narrow-to-subtree)
    (define-key map (kbd "C-j v w") #'widen)
    map) 
  "Keymap used when sbw-org-mode-minor-mode is active.")

(define-minor-mode sbw-org-mode-minor-mode
  "Custom minor mode for org-mode."
  :group   'sbw-modes
  :keymap  sbw-org-mode-minor-mode-keymap
  (if sbw-org-mode-minor-mode
    (sbw-bindings/ensure-mode-has-precedence 'sbw-org-mode-minor-mode-keymap)))

(add-hook 'org-mode-hook #'sbw-org-mode-minor-mode)

(provide 'sbw-bindings)
