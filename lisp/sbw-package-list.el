(setq sbw/package-list
      '(
        ;; Package management
        use-package ;; Easy package use

        ;; Elisp
        dash            ;; Modern list API
        dash-functional ;; Additional functions for dash
        f               ;; Modern file API
        s               ;; Modern string API
        seq             ;; Unified abstractions for sequences

        ;; Completion
        ivy     ;; Completion framework
        counsel ;; Ivy enhancements for common commands
        swiper  ;; Ivy alternative for search

        ;; Interface
        diminish  ;; Abbreviate minor mode indicators
        powerline ;; Emacs version of the Vim powerline

        ;; Parenthesis management
        lispy ;; Short and sweet Lisp editing
        
        ;; Usability
        key-chord ;; Map pairs of simultaneously pressed keys to commands

        ;; ;; Static checking
        ;; flyspell         ;; Spell checking
        ;; flyspell-correct ;; Correcting words with flyspell via custom interface
        ;; flycheck         ;; Syntax checking

        ;; ;; org-mode
        ;; org      ;; Your life in plain text
        ;; htmlize  ;; html org-mode reports
        ;; org-gcal ;; Org sync with Google Calendar

        ;; ;; Git
        magit ;; Control Git from Emacs
 
        ;; ;; Slack
        ;; slack   ;; Slack for Emacs
        ;; emojify ;; Emoji for Slack

        ;; ;; Languages

        ;; Haskell
        haskell-mode ;; Haskell mode
        intero       ;; Interactive Haskell development

        ;; ;; Elm
        ;; elm-mode     ;; Elm mode for emacs
        ;; flycheck-elm ;; Flycheck for Elm
     
        ;; ;; JSON
        ;; json-mode ;; Major mode for editing JSON files
        ))

(setq sbw/personal-package-list
      '(
        sbw-cosmetics
        sbw-cosmetics-code-style
        sbw-bindings
        sbw-utils
        ))

(provide 'sbw-package-list)
