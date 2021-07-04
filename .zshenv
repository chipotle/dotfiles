# Basic stuff

export MANPATH=/opt/brew/share/man:$MANPATH
export EDITOR=vim
export VISUAL=vim

# Homebrew config

export HOMEBREW_AUTO_UPDATE_SECS=14400
export HOMEBREW_EDITOR=bbedit
# export HOMEBREW_MAKE_JOBS=4

# Fix clang/Python fighting over warnings

export CFLAGS=-Qunused-arguments
export CPPFLAGS=-Qunused-arguments

