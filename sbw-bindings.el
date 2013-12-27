(defvar sbw-keys-mode-map (make-keymap) "sbw-keys-mode keymap.")

(define-minor-mode sbw-keys-mode
  "A minor mode to give my key binding precedence."
  t " SBW" 'sbw-keys-mode-map)

(sbw-keys-mode 1)

;; Key groups
;;  o   org-mode
;;  u   undo

;; org-mode
;;  a   open agenda
;;  t   set tags
;;  e   set effort
;;  s   set status
;;  c   schedule in calendar
;;  d   set deadline
;;  i   clock in
;;  o   clock out
;;  g   go to current task
;;  u   update all dynamic blocks
;;
;;  v   Change view
;;    n   narrow buffer to subtree
;;    w   widen buffer
(define-key sbw-keys-mode-map (kbd "C-j o a")   'org-agenda-list)
(define-key sbw-keys-mode-map (kbd "C-j o t")   'org-set-tags-command)
(define-key sbw-keys-mode-map (kbd "C-j o e")   'org-set-effort)
(define-key sbw-keys-mode-map (kbd "C-j o s")   'org-todo)
(define-key sbw-keys-mode-map (kbd "C-j o c")   'org-schedule)
(define-key sbw-keys-mode-map (kbd "C-j o d")   'org-deadline)
(define-key sbw-keys-mode-map (kbd "C-j o i")   'org-clock-in)
(define-key sbw-keys-mode-map (kbd "C-j o o")   'org-clock-out)
(define-key sbw-keys-mode-map (kbd "C-j o g")   'org-clock-goto)
(define-key sbw-keys-mode-map (kbd "C-j o u")   'org-update-all-dblocks)
(define-key sbw-keys-mode-map (kbd "C-j o v n") 'org-narrow-to-subtree)
(define-key sbw-keys-mode-map (kbd "C-j o v w") 'widen)
;(defun sbw/org-layout-tags () 
  ;(setq org-tags-column (- (window-width)))
  ;(org-align-all-tags))

;; undo
(define-key sbw-keys-mode-map (kbd "C-j u") 'undo-tree-visualize)

;; Ensure that bindings are done last
(defadvice load (after sbw-keys-mode-has-precedence)
  "Ensure that my keybindings have precedence."
  (if (not (eq (car (car minor-mode-map-alist)) 'sbw-keys-mode))
    (let ((mykeys (assq 'sbw-keys-mode minor-mode-map-alist)))
      (assq-delete-all 'sbw-keys-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(ad-activate 'load)

(provide 'sbw-bindings)
