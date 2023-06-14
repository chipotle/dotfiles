# Homebrew - test for both standard install locations

if [[ -a /opt/homebrew ]]; then
    eval $(/opt/homebrew/bin/brew shellenv)
elif [[ -a /usr/local/bin/brew ]]; then
    eval $(brew shellenv)
fi

if [[ `command -v brew` ]]; then
    export HOMEBREW_AUTO_UPDATE_SECS=3600
    export HOMEBREW_EDITOR=bbedit
    export HOMEBREW_NO_ENV_HINTS=1
    if [[ `command -v bat` ]]; then
        export HOMEBREW_BAT=1
    fi
fi

# rbenv

if [[ `command -v rbenv` ]]; then
    eval "$(rbenv init -)"
fi

# nvm

if [[ -a ~/.nvm ]]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
fi

# yarn

if [[ -a ~/.yarn ]]; then
    export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
fi

# pipx virtual environment

if [[ -a ~/.local/bin ]]; then
    export PATH="$PATH:$HOME/.local/bin"
fi

# Emacs bin directory

if [[ -a ~/.emacs.d/bin ]]; then
    export PATH="$PATH:$HOME/.emacs.d/bin"
fi
