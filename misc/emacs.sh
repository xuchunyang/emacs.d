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

info_in_emacs ()
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
alias dired=find_file

t ()
{
    emacsclient --eval "(progn (setq server-eval-and-how-to-print 'buffer) (org-agenda nil \"n\"))"
}

elisp_repl ()
{
    emacs --batch --eval '(while t (message "%s" (eval (read (read-string "> ")))))'
}

calc ()
{
    emacs -Q --batch --eval "(message \"%s\" (calc-eval \"$1\"))"
}

grep_in_emacs ()
{
    args=$*
    # For simplicity, prefix with -n/--line-number, -H/--with-filename and --color
    emacsclient --eval "(grep \"grep -nH --color $args\")"
    _open_emacs_window
}

rg_in_emacs ()
{
    pat=$1                      # regexp
    emacsclient --eval "(grep \"rg --smart-case --no-heading --line-number -e $pat\")"
    _open_emacs_window
}

git_grep_in_emacs ()
{
    pat=$1                      # basic regexp like Grep
    emacsclient --eval "(vc-git-grep \"$pat\" \"\" default-directory)"
    _open_emacs_window
}

man_in_emacs ()
{
    topic=$1
    emacsclient --eval "(man \"$topic\")"
    _open_emacs_window
}

shell_command ()
{
    cmd=$*
    emacsclient --eval "(shell-command \"$cmd\")"
    _open_emacs_window
}
