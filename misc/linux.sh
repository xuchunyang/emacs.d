# linux.sh --- Shell functions to invoke Emacs from the command line on GNU/Linux
#
# Created: Wed Nov  2 12:41:26 UTC 2016

magit ()
{
    local repo=$1
    (cd ${repo:=`pwd`} && emacsclient --eval '(magit-status)')
    _open_emacs_window
}

dired ()
{
    local arg1=$1
    local dir=${arg1:=`pwd`}
    emacsclient --eval "(dired \"$dir\")"
    _open_emacs_window
}

_open_emacs_window ()
{
    # Activate Emacs GUI Window
    xdotool windowactivate --sync $( xdotool search --class Emacs | tail -1 )
}

info ()
{
    local node=$1
    emacsclient --eval "(shell/info \"$node\")"
    _open_emacs_window
}
