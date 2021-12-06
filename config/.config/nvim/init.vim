set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

let g:blamer_enabled = 1
let g:blamer_date_format = '%m/%m/%y %H:%M'
set formatoptions+=jclroq

lua << EOF
require('lsp')
require('ts')
require('my_telescope')
require('nvim-autopairs').setup{}
require("nvim-treesitter.configs").setup {
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
  }
}
require("todo-comments").setup {}
require("trouble").setup {}
EOF
