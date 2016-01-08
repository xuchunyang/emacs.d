#!/usr/bin/osascript

set appName to "Emacs"

if application appName is running then
	tell application appName to quit
end if

-- This seems necessary to avoid crash
say "Restarting Emacs..."

tell application appName to activate
