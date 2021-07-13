# Editor setup

export EDITOR=vim
export VISUAL=vim

# Homebrew config - test for both standard install locations

if [[ -a /opt/homebrew ]]; then
    eval $(/opt/homebrew/bin/brew shellenv)
elif [[ -a /usr/local/bin/brew ]]; then
    eval $(brew shellenv)
fi

if [[ `command -v brew` ]]; then
    export HOMEBREW_AUTO_UPDATE_SECS=14400
    export HOMEBREW_EDITOR=bbedit
    if [[ `command -v bat` ]]; then
        export HOMEBREW_BAT=1
    fi
fi

# Python virtualenv setup (disabled)

# if [[ -a /usr/local/bin/virtualenvwrapper.sh ]]; then
#     export WORKON_HOME=~/src/envs
#     export PROJECT_HOME=~/src
#     source /usr/local/bin/virtualenvwrapper.sh
# fi

# rbenv initialization

if [[ `command -v rbenv` ]]; then
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
