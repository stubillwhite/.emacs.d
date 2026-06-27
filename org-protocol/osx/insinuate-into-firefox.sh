#!/bin/bash
FIREFOX_HOME="${HOME}/Library/Application Support/Firefox"
ORG_PROTOCOL_APP="/Users/white1/dev/my-stuff/.emacs.d/org-protocol/osx/org-protocol.app"

# Firefox invokes protocol handlers as direct binaries, but AppleScript .app bundles
# need to go through 'open'. This wrapper bridges the gap.
ORG_PROTOCOL_WRAPPER="${ORG_PROTOCOL_APP%/*}/org-protocol-handler.sh"

function confirm() {
    read -p "${1:-Are you sure? [y/n]} " response
    case "$response" in
        [Yy][Ee][Ss]|[Yy])
            true ;;
        *)
            false ;;
    esac
}

# Add or update a user_pref line in prefs.js
function firefox-set-config-setting() {
    local defaultsFile=$1
    local key=$2
    local value=$3
    if grep -q "\"${key}\"" "${defaultsFile}"; then
        sed -i '' "s|user_pref(\"${key}\".*);|user_pref(\"${key}\", ${value});|" "${defaultsFile}"
        printf "Updated:  %s = %s\n" "${key}" "${value}"
    else
        printf 'user_pref("%s", %s);\n' "${key}" "${value}" >> "${defaultsFile}"
        printf "Added:    %s = %s\n" "${key}" "${value}"
    fi
}

# Create a shell wrapper that Firefox can invoke directly, which in turn
# calls 'open' to properly launch the .app through macOS Launch Services
function create-wrapper() {
    cat > "${ORG_PROTOCOL_WRAPPER}" <<'WRAPPER'
#!/bin/bash
echo "$(date): invoked with args: $*" >> /tmp/org-protocol-debug.log
/usr/bin/open "$1"
WRAPPER
    chmod +x "${ORG_PROTOCOL_WRAPPER}"
    printf "Created:  %s\n" "${ORG_PROTOCOL_WRAPPER}"
}

# Fix the org-protocol entry in handlers.json to point at the wrapper
function fix-handlers-json() {
    local handlersFile=$1
    if [ ! -f "${handlersFile}" ]; then
        printf "No handlers.json found at %s, skipping\n" "${handlersFile}"
        return
    fi
    python3 - "${handlersFile}" "${ORG_PROTOCOL_WRAPPER}" <<'PYEOF'
import json, sys
path, bin_path = sys.argv[1], sys.argv[2]
with open(path) as f:
    data = json.load(f)
data['org-protocol'] = {"action": 2, "handlers": [{"name": "org-protocol", "path": bin_path}]}
with open(path, 'w') as f:
    json.dump(data, f, indent=2)
print(f"Updated:  handlers.json org-protocol -> {bin_path}")
PYEOF
}

echo 'WARNING: Ensure that Firefox is closed before running this script'
echo

# Sanity-check the .app exists
if [ ! -d "${ORG_PROTOCOL_APP}" ]; then
    echo "Error: org-protocol.app not found at ${ORG_PROTOCOL_APP}"
    exit 1
fi

# Create the wrapper script alongside the .app
create-wrapper

pushd "${FIREFOX_HOME}/Profiles" > /dev/null
IFS=$'\n'
for firefoxProfile in $(ls -1d ./*)
do
    confirm "Insinuate into ${firefoxProfile} [y/n]?" && {
        defaultsFile="${firefoxProfile}/prefs.js"
        handlersFile="${firefoxProfile}/handlers.json"
        cp -i "${defaultsFile}" "${defaultsFile}.bak"
        # expose=true: allow web content to navigate to this protocol (without this, clicks are silently dropped)
        # external=true: hand off to an external app rather than handling internally
        # app: shell wrapper that calls 'open', since Firefox can't invoke .app bundles directly
        firefox-set-config-setting "${defaultsFile}" 'network.protocol-handler.expose.org-protocol'   true
        firefox-set-config-setting "${defaultsFile}" 'network.protocol-handler.external.org-protocol' true
        firefox-set-config-setting "${defaultsFile}" 'network.protocol-handler.app.org-protocol'      "\"${ORG_PROTOCOL_WRAPPER}\""
        fix-handlers-json "${handlersFile}"
        echo
    }
done
popd > /dev/null
