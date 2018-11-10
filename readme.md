# Emacs configuration

Personal Emacs configuration, probably not very useful to others.

## Crib notes for things I always forget

### Haskell mode

#### Cabal projects

- Enable Dante instead of Intero
- Edit a Haskell file

    -- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
    -- [2,4]

- Evaluate block `C-c "`
- Type at point `C-c .`
- Info at point `C-c ,`

[Dev Env](http://haroldcarr.com/posts/2017-10-24-emacs-haskell-dev-env.html)
[Stuff](https://www.fosskers.ca/blog/nix-en.html)

#### Stack projects

`stack install hindent`

`stack install stylish-haskell` ? 
Doesn't seem good for me. Hindent looks better
/Users/white1/.local/bin/stylish-haskell -v src/Lib.hs

(setq haskell-mode-stylish-haskell-path "/Users/white1/.local/bin/stylish-haskell")
(custom-set-variables
 '(haskell-stylish-on-save t))
 
- From the command line run `stack build intero` to build the integration
