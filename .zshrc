# Setup zsh options

bindkey -e                                  # Emacs keybindings
unsetopt correct_all                        # zsh's autocorrect is weird
autoload -U colors && colors                # easy color names
autoload -U compinit                        # smart completion module
compinit
setopt prompt_subst                         # prompt substitution option
autoload -Uz vcs_info                       # zsh's version control integration
zstyle ":completion:*" menu yes select      # fancy completion menus

# History options

HISTFILE="$HOME/.zsh_history"
HISTSIZE=2000
SAVEHIST=2000
setopt hist_ignore_dups
setopt hist_expire_dups_first
setopt hist_verify
setopt inc_append_history
setopt share_history


# Git integration

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

# Prompt

PS1='${vcs_info_msg_0_}%{$fg[green]%}%m:%2~%#%{$reset_color%} '
# RPS1="%{$fg[yellow]%}%2~%{$reset_color%}"

# Environment variables and aliases

export EDITOR=vim
export VISUAL=vim
# export EDITOR="/opt/homebrew/bin/emacsclient -nw --alternate-editor=\"\""
# export VISUAL="/opt/homebrew/bin/emacsclient -nw --alternate-editor=\"\""

alias ls="ls -FG"
alias mct="mosh watts@coyotetracks.org -- tmux attach -d"
alias gpsup='git push --set-upstream origin $(git_current_branch)'
alias marked="open -a /Applications/Marked\ 2.app"
alias ec="emacsclient -n"
alias ect="emacsclient -nw"

# Functions

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

# Use bat (cat replacement) if present

if [[ `command -v bat` ]]; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
    export BAT_THEME="Dracula"
    batdiff() {
        git diff --name-only --diff-filter=d | xargs bat --diff
    }
fi

# Herd injected NVM configuration
export NVM_DIR="/Users/watts/Library/Application Support/Herd/config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

[[ -f "/Applications/Herd.app/Contents/Resources/config/shell/zshrc.zsh" ]] && builtin source "/Applications/Herd.app/Contents/Resources/config/shell/zshrc.zsh"

# Herd injected PHP 8.3 configuration.
export HERD_PHP_83_INI_SCAN_DIR="/Users/watts/Library/Application Support/Herd/config/php/83/"


# Herd injected PHP binary.
export PATH="/Users/watts/Library/Application Support/Herd/bin/":$PATH


# Herd injected PHP 8.4 configuration.
export HERD_PHP_84_INI_SCAN_DIR="/Users/watts/Library/Application Support/Herd/config/php/84/"

if [[ -d "/opt/homebrew/opt/chruby" ]]; then
    source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
    source /opt/homebrew/opt/chruby/share/chruby/auto.sh
fi
