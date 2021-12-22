set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

let g:blamer_enabled = 1
let g:blamer_date_format = '%m/%m/%y %H:%M'
let g:blamer_relative_time = 1
set formatoptions+=jclroq

augroup AutoChange
    autocmd!
    autocmd FileType python autocmd TextChanged,TextChangedI * :lua require('auto-change.python-f-strings').pythonFString()
    autocmd FileType javascript,typescript autocmd TextChanged,TextChangedI * :lua require('auto-change.ts-js-backticks').tsJsBackticks()
augroup END

lua << EOF
require('lsp')
require('ts')
require('my_telescope')
require('nvim-autopairs').setup{}
require("todo-comments").setup {}
require("trouble").setup {}
require('sort_object')
require('change_case')
EOF
