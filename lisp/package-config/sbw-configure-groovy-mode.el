(require 'use-package)

(use-package groovy-mode
  :mode
  ("\\.\\(groovy\\|gradle\\)$" . groovy-mode)

  :config
  (progn


    ;; Also check out http://www.emacswiki.org/emacs/IndentingC
    
;;; Commentary:

    ;; Provides the google C/C++ coding style. You may wish to add
    ;; `google-set-c-style' to your `c-mode-common-hook' after requiring this
    ;; file. For example:
    ;;
    ;;    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;;
    ;; If you want the RETURN key to go to the next line and space over
    ;; to the right place, add this to your .emacs right after the load-file:
    ;;
    ;;    (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;; Code:

    ;; For some reason 1) c-backward-syntactic-ws is a macro and 2)  under Emacs 22
    ;; bytecode cannot call (unexpanded) macros at run time:
    (eval-when-compile (require 'cc-defs))

    ;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
    ;; elegant solution of composing a list of lineup functions or quantities with
    ;; operators such as "add")
    (defun google-c-lineup-expression-plus-4 (langelem)
      "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\" of the Google
C++ Style Guide for the case where the previous line ends with an open
parenthese.

\"Current C expression\", as per the Google Style Guide and as clarified by
subsequent discussions,
means the whole expression regardless of the number of nested parentheses, but
excluding non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
      (save-excursion
        (back-to-indentation)
        ;; Go to beginning of *previous* line:
        (c-backward-syntactic-ws)
        (back-to-indentation)
        ;; We are making a reasonable assumption that if there is a control
        ;; structure to indent past, it has to be at the beginning of the line.
        (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
          (goto-char (match-end 1)))
        (vector (+ 4 (current-column)))))
        
    (defconst google-c-style
      `((c-recognize-knr-p . nil)
         (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
         (c-basic-offset . 2)
         (indent-tabs-mode . nil)
         (c-comment-only-line-offset . 0)
         (c-hanging-braces-alist . ((defun-open after)
                                     (defun-close before after)
                                     (class-open after)
                                     (class-close before after)
                                     (namespace-open after)
                                     (inline-open after)
                                     (inline-close before after)
                                     (block-open after)
                                     (block-close . c-snug-do-while)
                                     (extern-lang-open after)
                                     (extern-lang-close after)
                                     (statement-case-open after)
                                     (substatement-open after)))
         (c-hanging-colons-alist . ((case-label)
                                     (label after)
                                     (access-label after)
                                     (member-init-intro before)
                                     (inher-intro)))
         (c-hanging-semi&comma-criteria
           . (c-semi&comma-no-newlines-for-oneline-inliners
               c-semi&comma-inside-parenlist
               c-semi&comma-no-newlines-before-nonblanks))
         (c-indent-comments-syntactically-p . nil)
         (comment-column . 40)
         (c-cleanup-list . (brace-else-brace
                             brace-elseif-brace
                             brace-catch-brace
                             empty-defun-braces
                             defun-close-semi
                             list-close-comma
                             scope-operator))
         (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                              (func-decl-cont . ++)
                              (member-init-intro . ++)
                              (inher-intro . ++)
                              (comment-intro . 0)
                              (arglist-close . c-lineup-arglist)
                              (topmost-intro . 0)
                              (block-open . 0)
                              (inline-open . 0)
                              (substatement-open . 0)
                              (statement-cont
                                .
                                (,(when (fboundp 'c-no-indent-after-java-annotations)
                                    'c-no-indent-after-java-annotations)
                                  ,(when (fboundp 'c-lineup-assignments)
                                     'c-lineup-assignments)
                                  ++))
                              (label . /)
                              (case-label . +)
                              (statement-case-open . +)
                              (statement-case-intro . +) ; case w/o {
                              (access-label . /)
                              (innamespace . 0))))
      "Google C/C++ Programming Style")

    (defun google-set-c-style ()
      "Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
      (interactive)
      (make-local-variable 'c-tab-always-indent)
      (setq c-tab-always-indent t)
      (c-add-style "Google" google-c-style t))

    (defun google-make-newline-indent ()
      "Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
      (interactive)
      (define-key c-mode-base-map "\C-m" 'newline-and-indent)
      (define-key c-mode-base-map [ret] 'newline-and-indent))



    
    (defconst rlw-java-style
      '((c-basic-offset . 2)
         (c-comment-only-line-offset . (0 . 0))
         (c-comment-continuation-stars . "")
         (c-hanging-comment-starter-p . nil)
         (c-hanging-comment-ender-p . nil)
         (c-cleanup-list . (
                             list-close-comma
                             ))
         (c-offsets-alist . (
                              (arglist-intro . c-lineup-arglist-intro-after-paren)
                              (arglist-close . c-lineup-arglist)
                              (case-label . *)
                              (func-decl-cont . c-lineup-java-throws)
                              (inher-cont . c-lineup-java-inher)
                              (inline-open . 0)
                              (statement-case-open . +)
                              (statement-cont . *)
                              (substatement-open . 0)
                              ))
         (c-hanging-braces-alist . (
                                     (block-close . c-snug-do-while)
                                     (block-open after)
                                     (class-open after)
                                     (defun-open after)
                                     (inline-open after)
                                     (substatement-open after)
                                     ))
         (c-hanging-colons-alist . (
                                     (case-label after)
                                     ))
         )
      "RLW Java Programming Style")


    (defun rlw-java-mode-hook ()
      (c-add-style "RLW" rlw-java-style t)
      (setq tab-width 4
        indent-tabs-mode nil
        c-indent-comments-syntactically-p t)
      (c-toggle-auto-hungry-state 1)
      )
       
    (add-hook 'java-mode-hook 'rlw-java-mode-hook)
       
       
    (defun rlw-groovy-mode-hook ()
      (c-add-style "RLW" rlw-java-style t)
      (setq tab-width 4
        indent-tabs-mode nil
        c-indent-comments-syntactically-p t)
      (c-toggle-auto-hungry-state 1)
      )
       
    (add-hook 'groovy-mode-hook 'rlw-groovy-mode-hook)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defconst sbw/java-style
      `( (c-recognize-knr-p . nil) ;; No KnR declarations
         (c-basic-offset    . 4)   ;; Basic unit of offset
         (indent-tabs-mode  . nil) ;; Don't use tabs

         ;; Semicolon behaviour for auto newlines
         (c-hanging-semi&comma-criteria  . nil) ;; Semicolon should not insert newlines

         ;; Brace behaviour for auto newlines
         (c-hanging-braces-alist . ( (defun-open  after)     ;; After opening a function...
                                     (defun-block-intro nil) ;; ...but not in the first line...
                                     (defun-close nil)       ;; ...and not after closing
                                     (class-open  after)     ;; After opening a class...
                                     (class-close nil)       ;; ...but not after closing
                                     (inline-open after)     ;; After opening an inline method...
                                     (inline-close nil)      ;; ...but not after closing
                                     (block-open after)      ;; After opening a block...
                                     (block-close nil)       ;; ...but not after closing
                                     
                                     (string nil) ;; Not in multiline strings
                                     (c nil)      ;; Not in C comments
                                     )
           )
         ;; (c-comment-only-line-offset . 10)  ;; No extra indentation for start of comment lines
         
         
         
               
         
         
         )
      "SBW Java style")
    
    
    (defun sbw/set-java-style ()
      (interactive)
      (setq tab-width 4)
      (electric-pair-mode)
      (turn-off-smartparens-mode)
      (c-add-style "sbw-java" sbw/java-style t))


    
    )

    

  
  )

(provide 'sbw-configure-groovy-mode)
