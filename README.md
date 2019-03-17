My configuration files, stored on GitHub the way the cool kids do.

* The .zshrc file contains git integration without using Oh My Zsh.
* The .zshenv file has no PATH info, because that's not the best way to set your path on macOS (use /etc/paths and /etc/paths.d/ for that, so non-shell programs will also pick up changes).
* The .vimrc file is set up to run with no errors whether or not a few plugins are installed.