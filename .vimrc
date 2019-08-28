" vimrc - wm - 23 Aug 2019

" basics
set nocompatible
filetype plugin indent on
syntax on
set encoding=utf-8

" viminfo: # of files to save marks for, # of lines to save in each register,
" # of lines to save in search history, # of lines to save in cmd history
set viminfo='100,<100,/40,:40

" set separate swap directory
set directory^=~/.vim/swap//
set writebackup
set nobackup
set backupcopy=auto

" turn on omnicomplete
set omnifunc=syntaxcomplete#Complete

" automatically load changed files
set autoread

" mouse support
set mouse=a

" ignore whitespace in diff mode
set diffopt+=iwhite

" Be able to arrow key and backspace across newlines
set whichwrap=bs<>[]

" Enhance % matching navigation command
runtime macros/matchit.vim

" Status line
set statusline=%f\ %y%r%m
set statusline+=%=
" set statusline+=%{FugitiveStatusline()}
set statusline+=%c:%l/%L
set laststatus=2

" sane indenting
set autoindent
set smartindent
set copyindent

" sane tabbing
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=0

" sane line wrapping
set lbr

" scrolling and UI stuff
set incsearch
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
noremap \ ,

" ,,  toggle search highlighting
map <leader>, :set invhlsearch<CR>

" ,b  toggle background
map <leader>b :let &background = ( &background == "dark" ? "light" : "dark" )<CR>

" ,l  toggle line numbers
nmap <leader>l :set invnumber<CR>

" map up/down arrow keys to move by screen line
noremap <Up> gk
noremap <Down> gj
inoremap <Up> <Esc>gki
inoremap <Down> <Esc>gji

" map [b and ]b for buffer navigation
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

" expand %% to active buffer path on command line
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3

" EditorConfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" Dash
nmap <silent> <leader>h <Plug>DashSearch

" CtrlP
map <C-S-p> :CtrlPBuffer<CR>

