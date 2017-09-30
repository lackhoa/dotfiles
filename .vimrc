"Key maps for .tex files
"For these to work, you must put the name of the project (i.e. the name of the latex file without the '.tex' extension
" So what does this do? It saves the current vim file, compiles latex files (for as many times as it does). 
autocmd Filetype tex map <F2> :up <bar>!biber $(cat proj_name) && pdflatex $(cat proj_name).tex && pdflatex $(cat proj_name).tex <CR>

"This will read the pdf file:
autocmd Filetype tex map <F3> :!xreader $(cat proj_name).pdf <CR>

"Spell check
map <F6> :setlocal spell! spelllang=en_us<CR>

"Autocomplete brackets
imap { {}<Esc>i
imap ( ()<Esc>i
imap [ []<Esc>i 

"No more matching parentheses
let loaded_matchparen = 1

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
