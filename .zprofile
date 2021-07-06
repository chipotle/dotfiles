# Basic stuff

export EDITOR=vim
export VISUAL=vim

# Homebrew config

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