#!/bin/bash

echo 'Adding org-protocol:// to Chrome protocol handler allow-list'
defaults write com.google.Chrome URLAllowlist -array-add -string 'org-protocol://*'

echo 'Check chrome://policy/ in the browser to confirm that this has taken effect'
