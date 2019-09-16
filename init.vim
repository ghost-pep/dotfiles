" Fish doesn't play all that well with others
set shell=/bin/bash
let mapleader = "\<Space>"

" PLUGINS
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-commentary'
Plugin 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plugin 'dense-analysis/ale'
Plugin 'dag/vim-fish'
Plugin 'itchyny/lightline.vim'
Plugin 'machakann/vim-highlightedyank'
Plugin 'airblade/vim-rooter'
Plugin 'scrooloose/nerdtree'
Plugin 'rhysd/vim-clang-format'
Plugin 'ludovicchabant/vim-gutentags'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Quick-save
nmap <leader>w :w<CR>

filetype plugin indent on
set autoindent
set timeoutlen=300 " http://stackoverflow.com/questions/2154516/delay-before-o-opens-a-new-line
set encoding=utf-8
set scrolloff=2
set noshowmode
set hidden
set nowrap
set nojoinspaces

" Sane splits
set splitright
set splitbelow

set shiftwidth=4
set softtabstop=4
set tabstop=4
set noexpandtab

" MACOS ONLY
" Make ` be the escape key to avoid using the stupid touchbar
" inoremap ` <Esc>

"make esc do nothing"
" inoremap <Esc> <Nop>

" Proper search
set incsearch
set ignorecase
set smartcase
set gdefault

" Search results centered please
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz

" GUI setup
set guioptions-=T " Remove toolbar
set vb t_vb= " No more beeps
set backspace=2 " Backspace over newlines
set nofoldenable
set ruler " Where am I?
set ttyfast
" https://github.com/vim/vim/issues/1735#issuecomment-383353563
set lazyredraw
set synmaxcol=500
set laststatus=2
set relativenumber " Relative line numbers
set number " Also show current absolute line
set diffopt+=iwhite " No whitespace in vimdiff
" Make diffing better: https://vimways.org/2018/the-power-of-diff/
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set colorcolumn=80 " and give me a colored column
" make the color of the column be decent
highlight ColorColumn ctermbg=DarkGray
set showcmd " Show (partial) command in status line.
set mouse=a " Enable mouse usage (all modes) in terminals
set shortmess+=c " don't give |ins-completion-menu| messages.

" Show those damn hidden characters
" Verbose: set listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
set nolist
set listchars=nbsp:¬,extends:»,precedes:«,trail:•

" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>

" Use deoplete.
let g:deoplete#enable_at_startup = 1

" Clang format on write
let g:clang_format#auto_format = 1

" Use ALE for all completion
" call deoplete#custom#option('sources', {
" \ '_': ['ale'],
" \})

" switch windows easily
map <C-j> <C-w>j
map <C-h> <C-w>h
map <C-k> <C-w>k
map <C-l> <C-w>l

" Open NERDTree automatically
autocmd vimenter * NERDTree
" Close NERDTree automatically
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Help vim find tags
set tags=.git/tags,./.git/tags,tags,./tags
