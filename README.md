# .dotfiles

## zsh

`.zshrc` contains git integration without using Oh My Zsh.

`.zshenv` has no PATH info, because that's not the best way to set your path on macOS. Use `/etc/paths` and `/etc/paths.d/` for that, so non-shell programs pick up changes.

## tmux

The prefix command is bound to `^A`, not `^B`.

Vim `hjkl` keys are used to select panes and to resize with `^` held down.

Use `^A^A` for quick switching between panes.

## Vim

The configuration is relatively minimal and well-commented. The leader key is rebound to comma (`,`).

* `,,` toggles search highlighting
* `,b` toggles light/dark background
* `,d` toggles the NERDTree directory tree
* `,l` toggles line number display
* `,h` looks up the current word with Dash if it's installed

The up/down arrows are rebound to `gj` and `gk` to move by display line. (Instead of disabling arrow keys, why not just make them more useful?)

### Plugin installation

Run the `vim-setup.sh` script to install plugins under `.vim/pack/bundle`:

    ├── opt
    │   ├── gruvbox
    │   ├── space-vim-theme
    │   ├── vim-one
    │   └── vim-solarized8
    └── start
        ├── ctrlp.vim
        ├── dash.vim
        ├── editorconfig-vim
        ├── nerdtree
        ├── vim-endwise
        ├── vim-fugitive
        ├── vim-rails
        └── vim-surround

### Plugin updating

Run the `update.sh` script. This updates any plugin installed with `git clone`.

### Plugin philosophy

I'm trying to keep my plugins relatively minimal and as close to The Vim Way(tm) as possible; the guiding principle is to enhance rather than override. (NERDTree is arguably the exception to the rule I keep going back and forth on.)

## Coyote New.terminal

This is a terminal settings file for macOS's terminal, using the [Molokai][] color scheme.

[Molokai]: https://github.com/lysyi3m/macos-terminal-themes/blob/master/screenshots/molokai.png
