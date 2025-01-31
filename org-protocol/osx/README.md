# Setting up org-protocol handlers on OSX #

## Create an application ##

- Launch the Script Editor application and create a new document
- Copy-and-paste the following code into the new document
- Change "/usr/local/bin/emacsclient" to path to emacsclient on your system
- Save it in Application (not the default Script) File Format as org-protocol.app

    on open location this_URL
        do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient \"" & this_URL & "\""
    end open location

## Configure the application ##

- Navigate to the application (i.e., org-protocol.app) in the Finder, then right-click on it and
  select Show Package Contents. Now go into the Contents folder and open Info.plist
- Add the following code to the file, making sure to keep all existing key/string pairs intact:

    <key>CFBundleURLTypes</key>
    <array>
      <dict>
        <key>CFBundleURLName</key>
        <string>org-protocol handler</string>
        <key>CFBundleURLSchemes</key>
        <array>
          <string>org-protocol</string>
        </array>
      </dict>
    </array>

## Insinuiate into the browser ##

- Run `./insinuate-into-chrome.sh` to allow-list `org-protcol://` in Chrome
