" So what does this do? It saves the current vim file, compiles latex files twice and then output the pdf result
map <F2> :up <bar> !pdflatex *.tex && pdflatex *.tex && xreader *.pdf <CR>
map <F6> :setlocal spell! spelllang=en_us<CR>

"No more matching parentheses
let loaded_matchparen = 1

"No more pretending to be vi
:set nocp

"Highlight matching
:set hlsearch

