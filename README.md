# .dotfiles

My configuration files, stored on GitHub the way the cool kids do.

## Notes

### zsh

`.zshrc` contains git integration without using Oh My Zsh.

`.zshenv` has no PATH info, because that's not the best way to set your path on macOS. Use `/etc/paths` and `/etc/paths.d/` for that, so non-shell programs pick up changes.

### tmux

The prefix command is bound to `^A`, not `^B`.

Vim `hjkl` keys are used to select panes and to resize with `^` held down.

Use `^A^A` for quick switching between panes.

### Vim

The configuration is relatively minimal and well-commented. The leader key is rebound to comma (`,`). A few basic additions:

* `,,` toggles search highlighting
* `,b` toggles light/dark background
* `,l` toggles line number display
* `,h` looks up the current word with Dash if it's installed

The up/down arrows are set to move by display line rather than actual physical line (`gj` and `gk`). Instead of disabling arrow keys, why not just make them slightly more useful?

My "standard" set of plugins is installed with the `vim-setup.sh` script. I'm trying to keep them relatively minimal, and to stick with ones that enhance rather than override Vim's defaults.

* Color schemes: Solarized8, Vim One, Zenburn, Gruvbox
* Fugitive
* Surround
* Endwise
* Vim-Rails
* EditorConfig
* Dash
* CtrlP

I've taken [NERDTree][nt] out of my standard rotation for now to try to see if the "native" netrw will work just as well for me. CtrlP may not stick around if I can get used to the Vim Way(tm) for navigating directories and buffers, but it's *really* useful to someone coming from just about any other modern editor.

[Emmet][em] may come back if I do more serious HTML hacking.

[nt]: https://github.com/scrooloose/nerdtree
[em]: https://github.com/mattn/emmet-vim

