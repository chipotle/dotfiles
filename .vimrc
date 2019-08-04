" vimrc - wm - 29 Jul 2019

" disable VI compatibility
set nocompatible
filetype plugin indent on

" viminfo settings: save/restore buffer list, 8 file marks,
" 32 search patterns, 64 commands, 512 max register lines,
" limit registers to 64K, do not save highlights
set viminfo=%,'8,/32,:64,<512,s64,h

" set separate swap directory
set directory^=~/.vim/swap//
set writebackup
set nobackup
set backupcopy=auto

" turn on syntax highlighting
syntax on

" turn on omnicomplete
set omnifunc=syntaxcomplete#Complete

" automatically load changed files
set autoread

" set encoding
set encoding=utf-8

" mouse support
set mouse=a

" ignore whitespace in diff mode
set diffopt+=iwhite

" Be able to arrow key and backspace across newlines
set whichwrap=bs<>[]

" Status line
set statusline=%<
if exists(':Git')
    set statusline+=%{fugitive#statusline()}
endif
set statusline+=\ %f\ %m%w
set statusline+=%=
set statusline+=%y
set statusline+=\ %c\ %l/%L
set laststatus=2

" sane indenting
set autoindent
set smartindent
set copyindent

" sane tabbing
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=4

" sane line wrapping
set lbr

" scrolling and UI stuff
set incsearch
set hlsearch
set scrolljump=4
set scrolloff=2
set showcmd
set title
set history=200

" file finding and completion
set path+=**
set wildmenu
set wildignorecase

" set leader to , (comma)
let mapleader = ","

" leader mappings (not plugin-related)
map <leader>, :nohl<CR>
map <leader>b :let &background = ( &background == "dark" ? "light" : "dark" )<CR>
nmap <leader>l :set invnumber<CR>

" map up/down arrow keys to move by screen line
noremap <Up> gk
noremap <Down> gj
inoremap <Up> <Esc>gki
inoremap <Down> <Esc>gji

" === Plugin configuration ===

" NERDTree
let NERDTreeMouseMode=2
let NERDTreeChDirMode=2
let NERDTreeAutoDeleteBuffer=1
map <leader>d :NERDTreeToggle<CR>

" EditorConfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" Dash
nmap <silent> <leader>h <Plug>DashSearch

" CtrlP
map <C-S-p> :CtrlPBuffer<CR>
