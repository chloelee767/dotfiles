set number
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab

"remap escape to jk
inoremap jk <Esc> 

"use visual lines for navigation by default
nnoremap j gj
nnoremap k gk
nnoremap 0 g0
nnoremap $ g$

nnoremap gj j
nnoremap gk k
nnoremap g0 0
nnoremap g$ $

vnoremap j gj
vnoremap k gk
vnoremap 0 g0
vnoremap $ g$

vnoremap gj j
vnoremap gk k
vnoremap g0 0

vnoremap g$ $

let mapleader = " "

"Vim plug plugins: run :PlugInstall to install plugins
call plug#begin()
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Plug 'szymonmaszke/vimpyter'
Plug 'Konfekt/FastFold'
Plug 'sheerun/vim-polyglot'
Plug 'scrooloose/nerdtree'
"Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tpope/vim-surround'
Plug '~/Downloads/fzf'
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdcommenter'
call plug#end()

nmap <Leader>, :Buffers<CR>
nmap <Leader>' :Files<CR>
nmap <Leader>. :edit 
"nnoremap <C-;> <Leader>c<Space>

"let g:airline_theme='angr'
let g:airline_powerline_fonts=1
