# Emacs configuration

Personal Emacs configuration, probably not very useful to others.

## Installation notes ##

## Linux ###

- Install Emacs as you would any other package
- Clone and install [plexus/chemacs](https://github.com/plexus/chemacs) to allow multiple configurations
- Clone this repository

## OSX ###

Brew has removed configuration options `--with-cocoa` so now we have to use a special build

- Browse to [Emacs for Mac OSX](https://emacsformacosx.com/)
- Download and install the latest version
- Open `Finder` and locate the application, right-click, select `Open`, then confirm `Open` to authorize
- Clone and install [plexus/chemacs](https://github.com/plexus/chemacs) to allow multiple configurations
- Clone this repository

Rebind Emacs keys

- Ctrl-Cmd-Q by default will lock the screen and should be changed
   - Open System Preferences
   - Select `Keyboard` > `App Shortcuts`
   - Add a new shortcut with the menu title `Lock screen` and command shortcut Ctrl-Cmd-§ (or something equally unlikely)

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
