(require 'use-package)

(use-package elm-mode
  :defer t
  :diminish elm-indent-mode

  :config
  (progn
    ;; Disable conflicting minor modes
    (add-hook 'elm-mode-hook
              (lambda ()
                (yas-minor-mode -1)))

    ;; Autocomplete via Company
    (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
    (add-to-list 'company-backends 'company-elm)

    ;; Vertical split when splitting to the REPL
    (defun sbw/elm-mode--ensure-vertical-split (orig-fun &rest args)
      (let* ( (orig-width  split-width-threshold)
              (orig-height split-height-threshold) )
        (save-some-buffers t)
        (setq split-width-threshold  0
              split-height-threshold nil)    
        (apply orig-fun args)
        (setq split-width-threshold  orig-width
              split-height-threshold orig-height)))
    (advice-add 'elm-repl-load :around #'sbw/elm-mode--ensure-vertical-split)

    ;; Colorize elm-test output
    (defun sbw/elm-mode--colorize-elm-test-buffer ()
      (when (eq major-mode 'compilation-mode)
        (sbw/display-ansi-colors)))
    (add-hook 'compilation-filter-hook 'sbw/elm-mode--colorize-elm-test-buffer)

    ;; Ensure that tests can be run from src/ or tests/ directory
    (defun sbw/elm-mode--ignore-tests-dependency-file (orig-fun &rest args)
      (if-let ((result (apply orig-fun args)))
          (if (s-suffix? "tests/" result)
              (let ((default-directory (concat result "..")))
                (elm--find-dependency-file-path))
            result)))
    (advice-add 'elm--find-dependency-file-path :around #'sbw/elm-mode--ignore-tests-dependency-file))

  :bind
  (:map elm-mode-map
        ("<f5>" . elm-repl-load)))

(provide 'sbw-configure-elm-mode)


