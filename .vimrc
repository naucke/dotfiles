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
set smartcase
set incsearch
set nohlsearch
set mouse=
set spelllang=en,de
set tabstop=2
set shiftwidth=2
set expandtab
