set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'itchyny/lightline.vim'
call vundle#end()

let g:lightline = {'colorscheme': 'seoul256'}
hi Visual cterm=none ctermbg=darkgrey
syntax on

set spelllang=en,de
set title
set number relativenumber

set incsearch
set nohlsearch
set smartcase

set laststatus=2
set showcmd

filetype plugin indent on
set autoindent
set shiftwidth=2
set tabstop=2
