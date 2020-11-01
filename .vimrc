set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tmsvg/pear-tree'
Plugin 'itchyny/lightline.vim'

call vundle#end()
filetype plugin indent on

let g:lightline = {'colorscheme': 'solarized'}

syntax on
set laststatus=2
set number
set showcmd
set smartcase
set incsearch
set nohlsearch
set ttymouse=
set spelllang=en,de
set tabstop=2
set shiftwidth=2
set expandtab
