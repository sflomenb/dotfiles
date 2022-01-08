local action_set = require("telescope.actions.set")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local conf = require("telescope.config").values

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
set_keymap("n", "<Leader>o", "<cmd>lua require('telescope').extensions['todo-comments']['todo']()<cr>", opts)
set_keymap(
	"n",
	"<Leader>O",
	"<cmd>lua require('telescope').extensions['todo-comments']['todo']({ cwd = vim.fn.expand('%:h') })<cr>",
	opts
)

function _G.select_session(opts)
	opts = opts or {}
	pickers.new(opts, {
		prompt_title = "Find session",
		preview = false,
		finder = finders.new_oneshot_job({ "rg", "--files", "--hidden" }, { cwd = vim.fn.expand("~/.vim/sessions") }),
		sorter = conf.file_sorter(opts),
		attach_mappings = function(prompt_bufnr, map)
			actions.select_default:replace(function()
				local selection = action_state.get_selected_entry()
				if selection ~= nil then
					actions.close(prompt_bufnr)
					vim.api.nvim_command("source ~/.vim/sessions/" .. selection[1])
				end
			end)
			return true
		end,
	}):find()
end

vim.api.nvim_exec([[command! SelectSession :lua select_session()]], false)

-- Custom splitting to split in any direction.
-- Inspired by builtin actions and code from telescope.
local transform_mod = require("telescope.actions.mt").transform_mod

local directions = {
	["k"] = "new",
	["j"] = "bel new",
	["J"] = "bot bel new",
	["K"] = "top abo new",
	["h"] = "vnew",
	["l"] = "bel vnew",
	["L"] = "bot bel vnew",
	["H"] = "top abo vnew",
	["t"] = "tabedit",
}

-- or create your custom action
local split = transform_mod({
	split_in_direction = function(prompt_bufnr)
		local input = vim.fn.nr2char(vim.fn.getchar())
		print("input was " .. input)
		return action_set.edit(prompt_bufnr, directions[input] or "edit")
	end,
})

-- This is your opts table
require("telescope").setup({
	defaults = {
		mappings = {
			i = {
				["<C-s>"] = split.split_in_direction,
			},
		},
	},
	extensions = {
		["ui-select"] = {
			require("telescope.themes").get_dropdown({
				-- even more opts
			}),
		},
		["todo-comments"] = {},
	},
})
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require("telescope").load_extension("ui-select")
require("telescope").load_extension("todo-comments")
