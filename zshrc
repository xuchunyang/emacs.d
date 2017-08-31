#!/usr/bin/env zsh
# -*- symbolic-link-on-save-linkname: "~/.zshrc"; -*-

#--------------------------------- [ Helper ] ---------------------------------#

_source_maybe () {
    [[ -f "$1" ]] && source "$1"
}

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

#---------------------------------- [ fzf ] ----------------------------------#
_source_maybe /usr/share/fzf/key-bindings.zsh
_source_maybe /usr/share/fzf/completion.zsh

#---------------------------- [ Change Directory ] ----------------------------#
setopt autocd

#-------------------------- [ "Command not found" ] --------------------------#
_source_maybe /usr/share/doc/pkgfile/command-not-found.zsh

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

# .zshrc ends here
