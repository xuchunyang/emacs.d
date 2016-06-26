#!/bin/bash

# A BitBar <https://github.com/matryer/bitbar> plugin to display clocking task
# on OS X menu bar.

# Test if the server is running (see emacs variable 'server-socket-dir')
if [ -e "$TMPDIR/emacs$UID/server" ] || [ -e "/tmp/emacs$UID/server" ]; then
    emacsclient --eval '(chunyang-clocking-task)' | sed 's/^"\[\(.*\)\]"$/\1/'
else
    # Emacs server is not running
    echo "无所事事"
fi
