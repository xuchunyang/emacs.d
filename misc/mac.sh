# mac.sh --- Shell functions to invoke Emacs from shell on Mac OS
#
# Created: 2016-12-06

_open_emacs_window ()
{
    open -a Emacs
}

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

info ()
{
    local node=$1
    emacsclient --eval "(shell/info \"$node\")"
    _open_emacs_window
}
