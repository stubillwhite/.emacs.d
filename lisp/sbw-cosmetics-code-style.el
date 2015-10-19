;; C-style languages

;; Also check out http://www.emacswiki.org/emacs/IndentingC
    
(defconst sbw/java-style
  `( (c-recognize-knr-p . nil)     ;; No Kernighan and Ritchie declarations
     (c-basic-offset    . 4)       ;; Basic unit of offset
     (indent-tabs-mode  . nil)     ;; Don't use tabs

     ;; Semicolon behaviour for auto newlines
     (c-hanging-semi&comma-criteria  . nil) ;; Semicolon should not insert newlines

     ;; Colon behaviour for auto newlines
     (c-hanging-colons-alist . ( (case-label nil)            ;; Not after case labels
                                 (label nil)                 ;; Not after labels
                                 (access-label nil)          ;; Not after access labels
                                 (member-init-intro nil)     ;; Not after members
                                 (inher-intro nil)))         ;; Not after inherits
         
     ;; Brace behaviour for auto newlines
     (c-hanging-braces-alist . ( (defun-open after)           ;; After opening a function...
                                 (defun-block-intro nil)      ;; ...but not in the first line...
                                 (defun-close nil)            ;; ...and not after closing
                                 (class-open after)           ;; After opening a class...
                                 (class-close nil)            ;; ...but not after closing
                                 (inline-open after)          ;; After opening an inline method...
                                 (inline-close nil)           ;; ...but not after closing
                                 (block-open after)           ;; After opening a block...
                                 (block-close nil)            ;; ...but not after closing
                                 (brace-list-open after)      ;; After opening an array or enum...
                                 (brace-list-intro nil)       ;; ...but not in the first line...
                                 (brace-list-entry nil)       ;; ...or subsequent lines...
                                 (brace-list-close nil)       ;; ...or after closing
                                 (brace-entry-open after)     ;; After lines in an array or enum that start with brace
                                 (string nil)                 ;; Not in multiline strings
                                 (c nil)                      ;; Not in C comments
                                 )
       ))
  "sbw-java style")
        
(defun sbw/set-java-style ()
  "Set code style to sbw-java style"
  (interactive)
  (setq tab-width 4)
  (electric-pair-mode)
  (turn-off-smartparens-mode)
  (c-add-style "sbw-java" sbw/java-style t))

;; Shell scripts

(setq sh-basic-offset 2 ;; Basic indentation for shell scripts
      sh-indentation  2 ;; Basic indentation for shell scripts
      )

(provide 'sbw-cosmetics-code-style)
