"Vim plug plugins: run :PlugInstall to install plugins
call plug#begin()
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-sneak'
"Plug 'easymotion/vim-easymotion'

Plug 'scrooloose/nerdcommenter'

Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'sheerun/vim-polyglot'
Plug 'scrooloose/nerdtree'
Plug '~/Downloads/fzf'
Plug 'junegunn/fzf.vim'
call plug#end()

set number
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab

"use + register for clipboard
set clipboard+=unnamedplus

"remap jk to escape
inoremap jk <Esc> 

" emacs keybinds in insert mode
inoremap <C-b> <Left>
inoremap <C-f> <Right>
inoremap <C-E> <End>
inoremap <C-A> <Home>
inoremap <M-f> <Esc>wa
inoremap <M-b> <Esc>ba

let mapleader = " "

nnoremap <Leader>. :edit 
nnoremap <Leader>, :Buffers<CR>
nnoremap <Leader>' :Files<CR>
nnoremap <Leader>p :NERDTree<CR>

"map gs <Plug>(easymotion-prefix)
"nmap s <Plug>(easymotion-sn)
"xmap s <Plug>(easymotion-sn)
"omap z <Plug>(easymotion-sn)

let g:airline#extensions#tabline#enabled = 1
