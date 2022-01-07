set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'itchyny/lightline.vim'
call vundle#end()

let g:lightline = {'colorscheme': 'seoul256'}
filetype plugin indent on
syntax on

set expandtab
set incsearch
set laststatus=2
set nohlsearch
set number
set shiftwidth=2
set showcmd
set smartcase
set spelllang=en,de
set tabstop=2
set ttymouse=
