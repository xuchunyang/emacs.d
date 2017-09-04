#!/usr/bin/env zsh
# -*- symbolic-link-on-save-linkname: "~/.zshrc"; -*-

# XXX: Add Imenu Support for these sections

#--------------------------------- [ Helper ] ---------------------------------#
_source_maybe () {
    [[ -f "$1" ]] && source "$1"
}

#-------------------------- [ Environment Variable ] --------------------------#
export PATH="$HOME/bin:$HOME/src/scripts:$HOME/.emacs.d/bin:$HOME/.local/bin:$PATH"

# Enable CJK work-around of Xapina for Notmuch/Mu4e
export XAPIAN_CJK_NGRAM=1

#------------------------------ [ Extra files ] ------------------------------#
for file in ~/.{path,exports,aliases,functions,extra}; do
    _source_maybe "$file"
done
unset file

#--------------------------------- [ Prompt ] ---------------------------------#
PROMPT='%F{blue}%~%f $ '

# Prepend user@host when it's a SSH session
[[ -z "$SSH_CLIENT" ]] || PROMPT="%F{red}%n@%m%f $PROMPT"

# Disable zsh line editor for Tramp, (info "(tramp) Frequently Asked Questions")
[[ $TERM = "dumb" ]] && unsetopt zle && PROMPT='$ '

#------------------------------- [ Completion ] -------------------------------#
autoload -Uz compinit promptinit
compinit
promptinit

# Menu
zstyle ':completion:*' menu select

# Persistent rehash
zstyle ':completion:*' rehash true

#-------------------------------- [ History ] --------------------------------#
HISTSIZE=10000
HISTFILE="$HOME/.zsh_history"
SAVEHIST=$HISTSIZE

setopt hist_ignore_all_dups
setopt hist_ignore_space

setopt appendhistory
setopt sharehistory
setopt incappendhistory

#---------------------------------- [ fzf ] ----------------------------------#
# Arch
_source_maybe /usr/share/fzf/completion.zsh
_source_maybe /usr/share/fzf/key-bindings.zsh

# Mac
_source_maybe ~/src/fzf/shell/completion.zsh
_source_maybe ~/src/fzf/shell/key-bindings.zsh

#---------------------------- [ Change Directory ] ----------------------------#
setopt autocd

# z - https://github.com/rupa/z
if [[ $OSTYPE == darwin* ]]; then
    # MacPorts
    source /opt/local/etc/profile.d/z.sh
elif [[ $OSTYPE == linux* ]]; then
    # AUR https://aur.archlinux.org/packages/z
    source /usr/share/z/z.sh
fi

#-------------------------- [ "Command not found" ] --------------------------#
# Install pkgfile on Arch
[[ $OSTYPE == linux* ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

#---------------------------------- [ Help ] ----------------------------------#
# Use "run-help", or type M-h or ESC-h

# Better support for subcommand such as "git log"
autoload -Uz run-help-git
autoload -Uz run-help-ip
autoload -Uz run-help-openssl
autoload -Uz run-help-p4
autoload -Uz run-help-sudo
autoload -Uz run-help-svk
autoload -Uz run-help-svn

#--------------------------------- [ Alias ] ---------------------------------#
alias ls="command ls --color"
alias grep="command grep --color"

#--------------------------------- [ Proxy ] ---------------------------------#
chunyang-proxy () {
    setopt XTRACE
    export http_proxy=http://127.0.0.1:1087
    export https_proxy=http://127.0.0.1:1087
}

#--------------------------------- [ Emacs ] ---------------------------------#
source ~/.emacs.d/misc/emacs.sh 

#---------------------------------- [ Misc ] ----------------------------------#
local-ip () {
    if [[ Linux == "$(uname -s)" ]]; then
        # Strip the tailing space
        hostname --ip-addresses | awk '{ print $1 }'
    else
        ipconfig getifaddr en0
    fi
}

public-ip () {
    dig +short myip.opendns.com @resolver1.opendns.com
}

# For EXWM, later Emacs can set default-directory with it
precmd_set_terminal_title () {
    print -Pn "\e]2;$PWD\a"
}

if [[ $OSTYPE == linux* ]]; then
    precmd_functions=($precmd_functions precmd_set_terminal_title)
fi

# .zshrc ends here
