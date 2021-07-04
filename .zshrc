# ZSH Configuration / wm / 2019-Jul-23
# ====================================

# Setup zsh options

bindkey -e                                  # Emacs keybindings
unsetopt correct_all                        # zsh's autocorrect is weird
autoload -U colors && colors                # easy color names
autoload -U compinit                        # smart completion module
compinit
setopt prompt_subst                         # prompt substitution option
autoload -Uz vcs_info                       # zsh's version control integration

HISTFILE="$HOME/.zsh_history"               # reasonable history settings
HISTSIZE=5000
SAVEHIST=5000
setopt hist_ignore_dups
setopt hist_expire_dups_first
setopt hist_verify
setopt inc_append_history
setopt share_history

zstyle ":completion:*" menu yes select      # fancy completion menus

# Configure vcs_info

zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:*" formats "(%b%u%c)"
zstyle ":vcs_info:*" actionformats "(%b%u%c|%a)"
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" check-for-staged-changes true
zstyle ":vcs_info:*" stagedstr "%{$fg[red]%}^%{$reset_color%}"
zstyle ":vcs_info:*" unstagedstr "%{$fg[red]%}*%{$reset_color%}"

precmd() {
    vcs_info
}

# Set prompt

PS1='${vcs_info_msg_0_}%{$fg[green]%}%m:%2~%#%{$reset_color%} '
# RPS1="%{$fg[yellow]%}%2~%{$reset_color%}"

# Set up functions

function editdiff() {
    if [ -n "$1" ]
    then
        bbedit `git diff --name-only $1`
    else
        bbedit `git diff --name-only HEAD^1`
    fi
}

function git_current_branch() {
    echo `git branch | grep \* | cut -d ' ' -f2`
}

function pman() {
    man -t "$@" | open -f -a Preview
}

# Set a few aliases

alias ls="ls -FG"
alias mct="mosh watts@coyotetracks.org -- tmux attach -d"
alias gpsup='git push --set-upstream origin $(git_current_branch)'

# Python virtualenv setup

# if [[ -a /usr/local/bin/virtualenvwrapper.sh ]]; then
#     export WORKON_HOME=~/src/envs
#     export PROJECT_HOME=~/src
#     source /usr/local/bin/virtualenvwrapper.sh
# fi

# rbenv initialization

if [[ -a /usr/local/bin/rbenv ]]; then
    eval "$(rbenv init -)"
fi

# nvm initialization

if [[ -a ~/.nvm ]]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
fi

if [[ -a ~/.yarn ]]; then
    export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
fi
