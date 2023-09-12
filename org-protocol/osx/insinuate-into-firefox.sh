#!/bin/bash

FIREFOX_HOME="${HOME}/Library/Application Support/Firefox"

function confirm() {
    read -p "${1:-Are you sure? [y/n]} " response
    case "$response" in
        [Yy][Ee][Ss]|[Yy]) 
            true ;;
        *)
            false ;;
    esac
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

echo 'WARNING: Ensure that Firefox is closed before running this script'

pushd "${FIREFOX_HOME}/Profiles" > /dev/null

IFS=$'\n' && for firefoxProfile in $(ls -1d ./*)
do
    confirm "Insinuate into ${firefoxProfile} [y/n]?" ] && {
        defaultsFile=${firefoxProfile}/prefs.js 
        cp -i ${defaultsFile} ${defaultsFile}.bak

        firefox-add-config-setting "${defaultsFile}" 'network.protocol-handler.app.org-protocol'      '"/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"'
        firefox-add-config-setting "${defaultsFile}" 'network.protocol-handler.expose.org-protocol'   true
        firefox-add-config-setting "${defaultsFile}" 'network.protocol-handler.external.org-protocol' true

        echo
    }
done

popd > /dev/null


