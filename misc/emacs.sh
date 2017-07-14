# emacs.sh --- Terminal.app loves Emacs.app
#
# Terminal.app --> emacsclient(1) --> Emacs.app

magit ()
{
    local repo=$1
    (cd ${repo:=`pwd`} && emacsclient --eval '(magit-status)')
}

info-in-emacs ()
{
    local node=$1
    emacsclient --eval "(shell/info \"$node\")" --eval '(open-emacs-window)'
}

find-file ()
{
    local file=$1
    emacsclient --eval "(find-file \"$file\")" --eval '(open-emacs-window)'
}

alias ff=find-file
alias dired=find-file

elisp-repl ()
{
    emacs --batch --eval '(while t (message "%s" (eval (read (read-string "> ")))))'
}

calc ()
{
    emacs -Q --batch --eval "(message \"%s\" (calc-eval \"$1\"))"
}

grep-in-emacs ()
{
    args=$*
    # For simplicity, prefix with -n/--line-number, -H/--with-filename and --color
    emacsclient --eval "(grep \"grep -nH --color $args\")" --eval '(open-emacs-window)'
}

rg-in-emacs ()
{
    pat=$1                      # regexp
    emacsclient --eval "(grep \"rg --smart-case --no-heading --line-number -e $pat\")" \
                --eval '(open-emacs-window)'
}

git-grep-in-emacs ()
{
    pat=$1                      # basic regexp like Grep
    emacsclient --eval "(vc-git-grep \"$pat\" \"\" default-directory)" --eval '(open-emacs-window)'
}

man-in-emacs ()
{
    topic=$1
    emacsclient --eval "(man \"$topic\")" --eval '(open-emacs-window)'
}

shell-command ()
{
    cmd=$*
    emacsclient --eval "(shell-command \"$cmd\")" --eval '(open-emacs-window)'
}

# cd to default-directory of current-buffer of Emacs
cd-to-emacs ()
{
    cd "$( emacsclient --eval '(with-current-buffer (car (buffer-list)) (expand-file-name default-directory))' | tr -d '"' )"
}

recentf ()
{
    file=$( emacsclient --eval recentf-save-file | tr --delete '"' )
    grep --only-matching '".*"' "${file}" |
        tr --delete '"'                   |
        peco                              |
        xargs --delimiter "\n" emacsclient
}

org-agenda ()
{
    # Default to the Inbox
    cmdkey=${1-i}
    # NOTE: Assuming init-org.el has all Org Agenda setup, because loading a
    # whole init.el is slow
    emacs --batch --load ~/.emacs.d/init-org.el \
          --eval "(org-batch-agenda-csv \"$cmdkey\")" |
        csv-to-org-table                              |
        # Replace Org link with just the _description_ part
        sed -E -e 's/\[\[([^][]+)]\[([^][]+)]]/[4m\2[0m/' -e 's/\[\[([^][]+)]]/[4m\1[0m/'
}

# FIXME: Rewrite this
t ()
{
    emacsclient --eval "(progn (setq server-eval-and-how-to-print 'buffer) (org-agenda nil \"n\"))"
}
