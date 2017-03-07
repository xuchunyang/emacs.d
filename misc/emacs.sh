# emacs.sh --- Terminal.app loves Emacs.app
#
# Terminal.app --> emacsclient(1) --> Emacs.app

magit ()
{
    local repo=$1
    (cd ${repo:=`pwd`} && emacsclient --eval '(magit-status)')
}

info_in_emacs ()
{
    local node=$1
    emacsclient --eval "(shell/info \"$node\")" --eval '(open-emacs-window)'
}

find_file ()
{
    local file=$1
    emacsclient --eval "(find-file \"$file\")" --eval '(open-emacs-window)'
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
    emacsclient --eval "(grep \"grep -nH --color $args\")" --eval '(open-emacs-window)'
}

rg_in_emacs ()
{
    pat=$1                      # regexp
    emacsclient --eval "(grep \"rg --smart-case --no-heading --line-number -e $pat\")" \
                --eval '(open-emacs-window)'
}

git_grep_in_emacs ()
{
    pat=$1                      # basic regexp like Grep
    emacsclient --eval "(vc-git-grep \"$pat\" \"\" default-directory)" --eval '(open-emacs-window)'
}

man_in_emacs ()
{
    topic=$1
    emacsclient --eval "(man \"$topic\")" --eval '(open-emacs-window)'
}

shell_command ()
{
    cmd=$*
    emacsclient --eval "(shell-command \"$cmd\")" --eval '(open-emacs-window)'
}
