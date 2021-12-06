set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

let g:blamer_enabled = 1

lua << EOF
require('lsp')
require('ts')
require('my_telescope')
EOF
