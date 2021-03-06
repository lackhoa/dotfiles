set shell=/bin/bash

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'vim-surround'                 " Plugin to change surrounding quotes, parens...
Plugin 'vim-commentary'               " Plugin to comment
Plugin 'vim-scripts/indentpython.vim' " Plugin to indent python code
Plugin 'YouCompleteMe'                " code completion
Plugin 'Raimondi/delimitMate'         " auto-complete brackets
Plugin 'amdt/vim-niji'                " Rainbow
Plugin 'junegunn/vim-easy-align'      " Alignment

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

" Set comment character for file types below:
autocmd FileType python setlocal commentstring=#\ %s

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

"Key maps for .tex files
"For these to work, you must put the name of the project
"(i.e. the name of the latex file without the '.tex' extension
" So what does this do? It saves the current vim file, compiles latex file
autocmd Filetype tex map <F2> :!pdflatex (cat proj_name).tex <CR>

autocmd Filetype tex map <F4> :!biber (cat proj_name) <CR>

"This will read the pdf file:
autocmd Filetype tex map <F3> :!xreader (cat proj_name).pdf & <CR>

"Spell check
map <F6> :setlocal spell! spelllang=en_us<CR>

"No more pretending to be vi
:set nocp

"Highlight matching
:set hlsearch

"relative numbering
:set number relativenumber

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

" System clipboard:
:set clipboard=unnamedplus

"status line according to mode
" first, enable status line always
set laststatus=2

" now set it up to change the status line based on mode
if version >= 700
  au InsertEnter * hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=Red
  au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
endif


"=====[ Highlight matches when jumping to next ]=============

" This rewires n and N to do the highlighing...
nnoremap <silent> n   n:call HLNext(0.2)<cr>
nnoremap <silent> N   N:call HLNext(0.2)<cr>

" Blink the line containing the match...
function! HLNext (blinktime)
    set invcursorline
    redraw
    exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
    set invcursorline
    redraw
endfunction

"====[ Swap : and ; to make colon commands easier to type ]======

nnoremap  ;  :

"Fix pasting behavior (p -> P):
nnoremap p P

"Fix append behavior (a -> A):
nnoremap a A

"replace all occurences on one line by default (still needs to specify range for the entire file)
set gdefault

"Change j and k behavior
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

"=====================Python stuff=====================
au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" Sage settings (from Franco Saliola)
autocmd BufRead,BufNewFile *.sage,*.pyx,*.spyx set filetype=python
autocmd FileType python set makeprg=sage\ -b\ &&\ sage\ -t\ %
autocmd Filetype python set tabstop=4|set shiftwidth=4|set expandtab

" Highlight bad whitespace
highlight BadWhitespace ctermbg=red guibg=darkred
" match BadWhitespace "\s$"

" Highlight indentation parity:
highlight EightWhitespace ctermbg=darkgrey
autocmd Filetype python match EightWhitespace "\s\{8}"

" YCM: auto-complete goes away when you're done
let g:ycm_autoclose_preview_window_after_completion = 1

" ------------------------------------------
" Lisp settings
" ------------------------------------------
autocmd BufRead,BufNewFile *.scm,*.rkt,*.kar set filetype=lisp
autocmd Filetype lisp set tabstop=4|set shiftwidth=4|set expandtab
autocmd Filetype lisp match EightWhitespace "\s\{8}"

au Filetype lisp let b:delimitMate_quotes = "\""

highlight Quasiquote ctermfg=Green
autocmd Filetype lisp call matchadd('Quasiquote', "`")
highlight Quote ctermfg=Red
autocmd Filetype lisp call matchadd('Quote',"'")

" No more "copy" when deleting: contents deleted using 'x'
" goes to the black hole register
nnoremap x "_x

" Ctrl+j to add line below
nnoremap <silent><C-j> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent><C-k> :set paste<CR>m`O<Esc>``:set nopaste<CR>

" Turn off highlighting using Esc
map <esc><esc> :noh<cr>

let mapleader = " "
nnoremap <leader>t :tabnew<Enter>
" Navigating tabs
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
"Previous and next window
nnoremap <leader>w gt
nnoremap <leader>W gT

" Change parentheses highlighting style
hi MatchParen cterm=underline ctermbg=None ctermfg=cyan

" Show partial command
set showcmd

" Save all files with enter in Normal mode
nnoremap <CR> :wa<CR>

" S-k to join with line above
nnoremap <S-k> k<S-j>

nnoremap <Right> gt
nnoremap <Left> gT
nnoremap <Up> <C-y>
nnoremap <Down> <C-e>

" Turn indent off
filetype indent off

" Turn keyword highlighting off but keep niji color
syntax off
let g:niji_matching_filetypes = ['lisp', '', 'python']

" Line up with the line above
nnoremap <C-l> 0d^ky^jPl
inoremap <C-l> <Esc>0d^ky^jPli

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
