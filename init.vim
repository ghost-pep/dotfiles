" Fish doesn't play all that well with others
set shell=/bin/bash
let mapleader=' '

" PLUGINS
set nocompatible              " be iMproved, required

" set the runtime path to include Vundle and initialize
call plug#begin('~/.vim/plugged')

" let Vundle manage Vundle, required
Plug 'VundleVim/Vundle.vim'

" Gotta have it
Plug 'gruvbox-community/gruvbox'
Plug 'itchyny/lightline.vim'

" Hehe coc
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'tpope/vim-commentary'
Plug 'machakann/vim-highlightedyank'
Plug 'airblade/vim-rooter'
Plug 'scrooloose/nerdtree'
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-fugitive'

" delicious fuzzy
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'maxmellon/vim-jsx-pretty'
Plug 'vim-scripts/indentpython.vim'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'

call plug#end()

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
set updatetime=50
set encoding=utf-8
set scrolloff=2

" Hide the '--INSERT--' in the command bar area
set noshowmode
set hidden
set nobackup
set nowritebackup

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

" Persistent undo - undo/redo after closing vim
" if has('persistent_undo')
"     let undoDir = expand('$HOME/.config/nvim/undodir')
"     call mkdir(undoDir, 'p')
"     set undofile
"     set undodir=undoDir
" endif

if executable('rg')
    let g:rg_derive_root='true'
endif

" Sane splits
set splitright
set splitbelow

" Sane tabs
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab

" NetRW settings
let g:netrw_browse_split=2
let g:netrw_banner=0

" FZF settings
nmap <leader>r :Rg<CR>
nmap <leader><tab> :Files<CR>

" Vim Fugitive settings
nmap <leader>gs :G<CR>
nmap <leader>gf :diffget \\2<CR>
nmap <leader>gj :diffget \\3<CR>

" Coc Settings
" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" MACOS ONLY
" Make ` be the escape key to avoid using the stupid touchbar
if has('macunix')
    inoremap ` <Esc>
    "make esc do nothing
    inoremap <Esc> `
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

" switch windows easily
map <C-j> <C-w>j
map <C-h> <C-w>h
map <C-k> <C-w>k
map <C-l> <C-w>l

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
silent! colorscheme gruvbox

" Copy and paste to system clipboard with leader y and leader p
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

" A little bit of a bigger command window
set cmdheight=2

" Lightline config
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

" ====== RUST SUPPORT ======
" rustfmt command location
let g:rustfmt_command = '/Users/ghostpepper/.cargo/bin/rustfmt'
let g:rustfmt_autosave = 1

" ====== GO SUPPORT ======
" format with goimports instead of gofmt
let g:go_fmt_command = "goimports"
let g:go_fmt_autosave = 1

" ====== PYTHON SUPPORT ======
highlight BadWhitespace ctermbg=red guibg=red
au Filetype python
    \ setlocal tabstop=4
    \ | setlocal softtabstop=4
    \ | setlocal shiftwidth=4
    \ | setlocal textwidth=79
    \ | setlocal expandtab
    \ | setlocal autoindent
    \ | setlocal fileformat=unix
    \ | match BadWhitespace /\s\+$/
" Snakemake support
autocmd BufRead *.snake set filetype=conf

" ====== WEB DEV SUPPORT ======
au Filetype javascript,html,css 
      \ setlocal tabstop=2
      \ | setlocal softtabstop=2 
      \ | setlocal shiftwidth=2
