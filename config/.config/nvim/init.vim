set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

" disable mouse
set mouse=

let g:blamer_enabled = 0
let g:blamer_date_format = '%m/%m/%y %H:%M'
let g:blamer_relative_time = 1

" Auto wrap comments
set formatoptions+=jclroq

augroup AutoChange
    autocmd!
    autocmd FileType python autocmd TextChanged,TextChangedI <buffer=abuf> :lua require('auto-change.python-f-strings').pythonFString()
    autocmd FileType javascript,typescript autocmd TextChanged,TextChangedI <buffer=abuf> :lua require('auto-change.ts-js-backticks').tsJsBackticks()
augroup END

lua << EOF
-- color has to be first for some reason
require('color')
require('crates').setup()
require('lsp')
require('my_dap')
require('ts')
require('my_telescope')
require('npairs')
require("todo-comments").setup({
	keywords = {
		NOTE = { alt = { "INFO", "ASSUMPTION", "NB" } },
	},
})
require("trouble").setup {}
require('sort_object')
require('change_case')
require('prev_indent_level')
require('snip')
require('optional_chaining')
require('has_logger_var')
EOF
