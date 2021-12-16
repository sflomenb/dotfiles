local function set_keymap(...)
	vim.api.nvim_set_keymap(...)
end

local opts = { noremap = true, silent = true }

set_keymap("n", "<Leader>f", "<cmd>lua require('telescope.builtin').find_files({ hidden = true })<cr>", opts)
set_keymap(
	"n",
	"<Leader>F",
	"<cmd>lua require('telescope.builtin').find_files({ hidden = true, cwd = vim.fn.expand('%:h') })<cr>",
	opts
)
set_keymap("n", "<Leader>g", "<cmd>lua require('telescope.builtin').git_files()<cr>", opts)
set_keymap(
	"n",
	"<Leader>G",
	"<cmd>lua require('telescope.builtin').git_files({ cwd = vim.fn.expand('%:h') })<cr>",
	opts
)
set_keymap("n", "<Leader>b", "<cmd>lua require('telescope.builtin').buffers()<cr>", opts)
set_keymap(
	"n",
	"<Leader>k",
	"<cmd>lua require('telescope.builtin').grep_string({ hidden = true, use_regex = true })<cr>",
	opts
)
set_keymap(
	"n",
	"<Leader>K",
	"<cmd>lua require('telescope.builtin').grep_string({ hidden = true, use_regex = true, cwd = vim.fn.expand('%:h') })<cr>",
	opts
)
set_keymap(
	"n",
	"<Leader>e",
	"<cmd>lua require('telescope.builtin').live_grep({ additional_args = function(opts) return {'--hidden'} end })<cr>",
	opts
)
set_keymap(
	"n",
	"<Leader>E",
	"<cmd>lua require('telescope.builtin').live_grep({ additional_args = function(opts) return {'--hidden'} end, cwd = vim.fn.expand('%:h') })<cr>",
	opts
)
