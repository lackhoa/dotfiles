"Key maps for .tex files
" So what does this do? It saves the current vim file, compiles latex files twice and then output the pdf result
autocmd Filetype tex map <F2> :up <bar> !pdflatex *.tex <CR>
"This will read the pdf file:
autocmd Filetype tex map <F3> :!xreader *.pdf <CR>

"Spell check
map <F6> :setlocal spell! spelllang=en_us<CR>

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

