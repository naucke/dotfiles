set nocompatible

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'itchyny/lightline.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-fugitive'

call vundle#end()
filetype plugin indent on

syntax on
set number
set wildmenu
set laststatus=2
set incsearch
set ignorecase

let g:lightline = {'colorscheme': 'solarized'}
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

set expandtab
set softtabstop=4
