"Vim plug plugins: run :PlugInstall to install plugins
call plug#begin()
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-sneak'
"Plug 'sheerun/vim-polyglot'

Plug 'preservim/nerdtree'
Plug 'tpope/vim-commentary'
call plug#end()

syntax on
set number
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab
set autoindent
set smartindent
set wildmenu
set mouse=a

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

nnoremap <Leader><Space> :
vnoremap <Leader><Space> :
nnoremap <Leader>. :edit<Space>
nnoremap <Leader>, :buf<Space>
nnoremap <Leader>fs :w<CR>
nnoremap <Leader>qq :qa<CR>

nnoremap <Leader>op :NERDTreeToggle<CR>
nnoremap <Leader>cc :Commentary<CR>

" Buffers
nnoremap <Leader>bn :bn<CR>
nnoremap <Leader>bp :bp<CR>
nnoremap <Leader>bk :bd<CR>
nnoremap [b :bp<CR>
nnoremap ]b :bn<CR>
nnoremap <Leader>bb :buffers<CR>
"close all buffers
nnoremap <Leader>bK :bufdo bd<CR> 
"close all buffers except the current one
"note that we need to escape the pipe
nnoremap <Leader>bO :%bd\|e#<CR>

" Tabs
nnoremap <leader>tn :tabnext<CR>
nnoremap <leader>tp :tabprevious<CR>
nnoremap [t :tabprevious<CR>
nnoremap ]t :tabnext<CR>
nnoremap <leader>tN :tabnew<CR>
nnoremap <leader>tk :tabclose<CR>
nnoremap <leader>tm :tabmove
nnoremap <leader>tO :tabonly<CR>
