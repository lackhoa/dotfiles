set nocompatible              " required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" add all your plugins here (note older versions of Vundle
" used Bundle instead of Plugin)
Plugin 'vim-surround'	" Plugin to change surrounding quotes, parens...
Plugin 'vim-commentary'	" Plugin to comment
" Set comment character for file types below:
autocmd FileType python setlocal commentstring=#\ %s

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

"Key maps for .tex files
"For these to work, you must put the name of the project (i.e. the name of the latex file without the '.tex' extension
" So what does this do? It saves the current vim file, compiles latex file
autocmd Filetype tex map <F2> :!pdflatex (cat proj_name).tex <CR>

autocmd Filetype tex map <F4> :!biber (cat proj_name) <CR>

"This will read the pdf file:
autocmd Filetype tex map <F3> :!xreader (cat proj_name).pdf & <CR>

"Spell check
map <F6> :setlocal spell! spelllang=en_us<CR>

"Autocomplete brackets
"imap { {}<Esc>i
"imap ( ()<Esc>i
"imap [ []<Esc>i

"Disable matching parentheses
" let loaded_matchparen = 1

"No more pretending to be vi
:set nocp

"Highlight matching
:set hlsearch

"weird numbering
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
  au InsertEnter * hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=Magenta
  au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
endif


"=====[ Highlight matches when jumping to next ]=============

" This rewires n and N to do the highlighing...
nnoremap <silent> n   n:call HLNext(0.2)<cr>
nnoremap <silent> N   N:call HLNext(0.2)<cr>

" EITHER blink the line containing the match...
function! HLNext (blinktime)
    set invcursorline
    redraw
    exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
    set invcursorline
    redraw
endfunction

"====[ Swap : and ; to make colon commands easier to type ]======

nnoremap  ;  :

"====[ Make tabs, trailing whitespace, and non-breaking spaces visible ]======
" For some reason the line below doesn't work
" exec "set listchars=tab:>_,eol:\⏎,trail:\uB7,nbsp:~"

" So this is the fix: Tab identified by a bar and trailing spaces identified by middle dot
:set lcs=trail:·,space:·,nbsp:~,tab:\|\ 

" If you can handle it, set it on default
set list

"Fix pasting behavior (p -> P):
nnoremap p P

"Fix append behavior (a -> A):
nnoremap a A

"replace all occurences on one line by default (still needs to specify range for the entire file)
set gdefault

""""""
" Sage settings (from Franco Saliola)
autocmd BufRead,BufNewFile *.sage,*.pyx,*.spyx set filetype=python
autocmd Filetype python set tabstop=4|set shiftwidth=4|set expandtab
autocmd FileType python set makeprg=sage\ -b\ &&\ sage\ -t\ %

" Ctrl + a for select all:
:map <C-a> GVgg

"Change j and k behavior
nnoremap j gj
nnoremap k gk

nnoremap gj j
nnoremap gk k

au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" No more "copy" when deleting: contents deleted using 'x'
" goes to the black hole register
nnoremap x "_x

" Fold python code
set foldmethod=indent

" Show matching parenthesis:
" :set showmatch
" :set matchtime=2
" Highlight text in between matching parentheses:
" noremap % v%

" Ctrl+j to add line below and Ctrl+k to add line above
nnoremap <silent><C-j> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent><C-k> :set paste<CR>m`O<Esc>``:set nopaste<CR>

" textwidth constraint
:set textwidth=80

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
