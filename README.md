# Emacs configuration

Personal Emacs configuration, probably not very useful to others.

TODO:

- Move to https://github.com/progfolio/elpaca
- Look at https://orgmode.org/worg/org-contrib/org-mac-link.html
- Look at https://github.com/ndwarshuis/org-ml
- Look at https://github.com/victorhge/iedit
- Look at https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/

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
- Clone and install [plexus/chemacs2](https://github.com/plexus/chemacs2) to allow multiple configurations
- Clone this repository

Rebind Emacs keys

- Ctrl-Cmd-Q by default will lock the screen and should be changed
   - Open System Preferences
   - Select `Keyboard` > `App Shortcuts`
   - Add a new shortcut with the menu title `Lock Screen` and command shortcut Ctrl-Cmd-§ (or something equally 
     unlikely). Be careful with the name of the command -- it must exactly match the name in the Apple menu.

