# emacs.sh --- Terminal.app loves Emacs.app
#
# Terminal.app --> emacsclient(1) --> Emacs.app

OS=$( uname -s )

if [ "$OS" == "Darwin" ]; then
    _open_emacs_window ()
    {
        # Multiple Emacs.app are installed on my Mac, and a running one is prefered.
        EMACS=Emacs
        PID=$( pgrep Emacs ) &&
            # Note that both GNU grep and BSD grep works
            EMACS=$( ps -p $PID | grep --extended-regexp --only-matching '/.*/Emacs(Mac)?.app' )
        open -a $EMACS
    }
elif [ "$OS" == "Linux" ]; then
    _open_emacs_window ()
    {
        # Activate Emacs GUI Window
        xdotool windowactivate --sync $( xdotool search --class Emacs | tail -1 )
    }
else
    _open_emacs_window ()
    {
        echo "Warning: Unsupport OS \"$OS\", do not know how to open Emacs.app"
    }
fi

magit ()
{
    local repo=$1
    (cd ${repo:=`pwd`} && emacsclient --eval '(magit-status)')
    _open_emacs_window
}

info ()
{
    local node=$1
    emacsclient --eval "(shell/info \"$node\")"
    _open_emacs_window
}

find_file ()
{
    local file=$1
    emacsclient --eval "(find-file \"$file\")"
    _open_emacs_window
}

alias ff=find_file

t ()
{
    emacsclient --eval "(progn (setq server-eval-and-how-to-print 'buffer) (org-agenda-list))"
}

elisp_repl ()
{
    emacs --batch --eval '(while t (message "%s" (eval (read (read-string "> ")))))'
}

calc ()
{
    emacs -Q --batch --eval "(message \"%s\" (calc-eval \"$1\"))"
}
