# ZSH Configuration / wm / 2019-Mar-18
# ====================================

# Setup and zsh options

bindkey -e                                  # Emacs keybindings
unsetopt correct_all                        # zsh's autocorrect is weird
autoload -U colors && colors                # easy color names
autoload -U compinit                        # smart completion module
compinit
setopt prompt_subst                         # prompt substitution option
autoload -Uz vcs_info                       # zsh's version control integration

HISTSIZE=1000                               # reasonable history settings
SAVEHIST=1000
setopt append_history
setopt hist_ignore_dups

zstyle ":completion:*" menu yes select      # fancy completion menus

# Configure vcs_info

zstyle ":vcs_info:*" enable git svn
zstyle ":vcs_info:*" formats "(%s:%b%u%c)"
zstyle ":vcs_info:*" actionformats "(%s:%b%u%c|%a)"
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" check-for-staged-changes true
zstyle ":vcs_info:*" stagedstr "%{$fg[red]%}^%{$reset_color%}"
zstyle ":vcs_info:*" unstagedstr "%{$fg[red]%}*%{$reset_color%}"

precmd() {
    vcs_info
}

# Set prompt

# PS1='${vcs_info_msg_0_}%{$fg[green]%}%m@%{$fg[yellow]%}%1~%# %{$reset_color%}'
PS1='${vcs_info_msg_0_}%{$fg[green]%}%m%#%{$reset_color%} '
RPS1="%{$fg[yellow]%}%2~%{$reset_color%}"

# Set up a couple functions

function editdiff() {
    bbedit `git diff --name-only $1`
}

function git_current_branch() {
    echo `git branch | grep \* | cut -d ' ' -f2`
}

# Set a few aliases

alias ls="ls -F"
alias mct="mosh coyotetracks.org -- tmux attach -d"
alias less=most
alias gpsup='git push --set-upstream origin $(git_current_branch)'

# Python virtualenv setup

export WORKON_HOME=~/src/envs
export PROJECT_HOME=~/src
source /usr/local/bin/virtualenvwrapper.sh
