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

* `,b` toggles background between light and dark
* `,d` toggles NERDTree if it's installed
* `,h` looks up the current word with Dash if it's installed
* `,,` turns off highlighting if it's on
* Use `C-S-j`/`C-S-k` to move line(s) up and down

My current plugin tree, using Vim 8's native manager:

```
opt
├── TuttiColori-Colorscheme
├── alchemist.vim
├── gruvbox
├── jellybeans.vim
├── vim-one
├── vim-rails
├── vim-solarized8
└── zenburn
start
├── ctrlp.vim
├── dash.vim
├── editorconfig-vim
├── emmet-vim
├── nerdtree
├── vim-elixir
├── vim-endwise
├── vim-fugitive
└── vim-surround
```
