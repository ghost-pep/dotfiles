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
Plugin 'rust-lang/rust.vim'
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
Plugin 'majutsushi/tagbar'
Plugin 'chriskempson/base16-vim'

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

" Set the path to recurse search for files lower in a project
set path+=**

" Display all matching files when trying to tab complete finding files
set wildmenu
set wildmode=list:longest
set wildignore=.hg,.svn,*~,*.png,*.jpg,*.gif,*.settings,Thumbs.db,*.min.js,*.swp,publish/*,intermediate/*,*.o,*.hi,Zend,vendor

set autoindent
set timeoutlen=300 " http://stackoverflow.com/questions/2154516/delay-before-o-opens-a-new-line
set updatetime=300
set encoding=utf-8
set scrolloff=2

" Hide the '--INSERT--' in the command bar area
set noshowmode
set hidden

" wrapping settings
set wrap
set linebreak
set textwidth=100
set wrapmargin=100
set nojoinspaces

" Wrapping options
set formatoptions=tc " wrap text and comments using textwidth
set formatoptions+=r " continue comments when pressing ENTER in I mode
set formatoptions+=q " enable formatting of comments with gq
set formatoptions+=n " detect lists for formatting
set formatoptions+=b " auto-wrap in insert mode, and do not wrap old long lines

" Sane splits
set splitright
set splitbelow

set shiftwidth=8
set softtabstop=8
set tabstop=8
set expandtab

" MACOS ONLY
" Make ` be the escape key to avoid using the stupid touchbar
if has('macunix')
    inoremap ` <Esc>
    "make esc do nothing
    inoremap <Esc> <Nop>
endif

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
set colorcolumn=100 " and give me a colored column
" make the color of the column be decent
highlight ColorColumn ctermbg=DarkGray
set showcmd " Show (partial) command in status line.
set mouse=a " Enable mouse usage (all modes) in terminals
set shortmess+=c " don't give |ins-completion-menu| messages.

" Show those damn hidden characters
set nolist
" The following is the non-verbose one:
" set listchars=nbsp:¬,extends:»,precedes:«,trail:•
set listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•

" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>

" Use deoplete.
let g:deoplete#enable_at_startup = 1

" use rust ale linters
let g:ale_linters = {'rust': ['cargo', 'rustfmt', 'rls']}

let g:rustfmt_autosave = 1

" switch windows easily
map <C-j> <C-w>j
map <C-h> <C-w>h
map <C-k> <C-w>k
map <C-l> <C-w>l

" rustfmt command location
let g:rustfmt_command = '/Users/ghostpepper/.cargo/bin/rustfmt'

" Open NERDTree automatically
" autocmd vimenter * NERDTree
" Close NERDTree automatically
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" show hidden files in NERDTree
let NERDTreeShowHidden=1

" Help vim find tags
set tags=.git/tags,./.git/tags,./tags

" 256 colors for vim
let base16colorspace=256
silent! colorscheme base16-gruvbox-dark-medium

" Copy and paste to system clipboard with leader y and leader p
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

" A little bit of a bigger command window
set cmdheight=2
