set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

let g:blamer_enabled = 1
set formatoptions+=jclroq

lua << EOF
require('lsp')
require('ts')
require('my_telescope')
require('nvim-autopairs').setup{}
EOF
