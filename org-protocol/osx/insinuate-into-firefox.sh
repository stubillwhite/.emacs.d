#!/bin/bash

FIREFOX_HOME="${HOME}/Library/Application Support/Firefox"

function get-firefox-profile() {
    if [[ $(grep '\[Profile[^0]\]' profiles.ini) ]]; then 
        profilePath=$(grep -E '^\[Profile|^Path|^Default' profiles.ini | grep -1 '^Default=1' | grep '^Path' | gcut -c6-)
    else 
        profilePath=$(grep 'Path=' profiles.ini | gsed 's/^Path=//')
    fi
    echo "${profilePath}"
}

function firefox-add-config-setting() {
    local defaultsFile=$1
    local key=$2
    local value=$3

    if grep ${key} ${defaultsFile} > /dev/null ; then
        gprintf "Setting ${key} already present in ${defaultsFile}\n"
    else
        gprintf "user_pref(\"${key}\", ${value});\n" >> "${defaultsFile}"
    fi
}

pushd "${FIREFOX_HOME}" > /dev/null

firefoxProfile=$(get-firefox-profile)

defaultsFile=${firefoxProfile}/prefs.js 
cp -i ${defaultsFile} ${defaultsFile}.bak

firefox-add-config-setting "${defaultsFile}" 'network.protocol-handler.app.org-protocol'      '"/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"'
firefox-add-config-setting "${defaultsFile}" 'network.protocol-handler.expose.org-protocol'   true
firefox-add-config-setting "${defaultsFile}" 'network.protocol-handler.external.org-protocol' true

popd > /dev/null

