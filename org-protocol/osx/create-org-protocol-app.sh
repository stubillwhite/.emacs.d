#!/bin/bash
# Creates OrgProtocolHandler.app — a minimal AppleScript app Firefox can invoke
# as a protocol handler.
#
# Firefox invokes protocol handlers via macOS GURL Apple Events, not CLI args,
# so this must be a proper .app bundle. It receives the URL via the AppleScript
# 'on open location' handler and calls emacsclient directly.
#
# $TMPDIR is set via 'getconf DARWIN_USER_TEMP_DIR' so emacsclient can find
# the Emacs server socket regardless of the shell environment.
#
# Deliberately has no CFBundleURLTypes so macOS doesn't register it as a
# scheme handler, keeping org-protocol.app as the system-level handler.

APP_PATH="/Users/white1/dev/my-stuff/.emacs.d/org-protocol/osx/OrgProtocolHandler.app"

echo "Building ${APP_PATH}..."

# Remove any previous version so osacompile gets a clean slate
rm -rf "${APP_PATH}"

# Compile the AppleScript app
osacompile -o "${APP_PATH}" - <<'APPLESCRIPT'
on open location theURL
    do shell script "TMPDIR=$(getconf DARWIN_USER_TEMP_DIR) /Applications/Emacs.app/Contents/MacOS/bin/emacsclient \"" & theURL & "\""
end open location
APPLESCRIPT

# Verify osacompile actually created the binary
if [ ! -f "${APP_PATH}/Contents/MacOS/applet" ]; then
    echo "Error: osacompile did not create Contents/MacOS/applet"
    echo "Contents of MacOS dir:"
    ls -la "${APP_PATH}/Contents/MacOS/" 2>/dev/null || echo "  (missing)"
    exit 1
fi
echo "osacompile succeeded — applet binary created"

# Keep osacompile's Info.plist intact — replacing it strips required keys
# like CFBundlePackageType that macOS needs to launch the app.
# Just patch in a stable bundle identifier.
/usr/libexec/PlistBuddy -c "Set :CFBundleIdentifier org.emacs.org-protocol-handler" \
    "${APP_PATH}/Contents/Info.plist"

# Ad-hoc code sign so macOS Launch Services can resolve the executable
codesign --force --deep --sign - "${APP_PATH}"

echo "Done. In Firefox Settings > Files and applications:"
echo "  1. Click the org-protocol dropdown"
echo "  2. Choose 'Other...' and select:"
echo "     ${APP_PATH}"
