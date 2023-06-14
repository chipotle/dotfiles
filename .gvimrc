" MacVim-specific configuration

set guifont=MonoLisa:h12
set lines=50
set columns=120
set linespace=3
colorscheme space_vim_theme
let g:netrw_browsex_viewer = "/usr/bin/open"
set number
set macligatures

func! ChangeBackground()
    if (v:os_appearance == 1)
        set background=dark
    else
        set background=light
    endif
    redraw!
endfunc

au OSAppearanceChanged * call ChangeBackground()

