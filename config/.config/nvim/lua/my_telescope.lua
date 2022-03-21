local action_set = require("telescope.actions.set")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local conf = require("telescope.config").values
local my_split_directions = require("split")

local function set_keymap(...)
	vim.api.nvim_set_keymap(...)
end

local opts = { noremap = true, silent = true }

-- TODO: Change these to use vim.api.nvim_set_keymap
set_keymap("n", "<Leader>tr", "<cmd>lua require('telescope.builtin').resume({ hidden = true })<cr>", opts)
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
set_keymap("n", "<Leader>b", "<cmd>lua require('my_telescope').my_buffers()<cr>", opts)
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
vim.api.nvim_set_keymap("n", "<space>fb", ":Telescope file_browser<CR>", { noremap = true })

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

local edit_directions = {
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
		return action_set.edit(prompt_bufnr, edit_directions[input] or "edit")
	end,
	-- HACK: We need a second action just for opening a buffer in a split since
	-- there is validation that only lets you execute certain commands to open
	-- the buffer in a split. This doesn't seem to exist for the other commands.
	buffer_split_in_direction = function(prompt_bufnr)
		-- action_state.get_current_history():append(
		-- 	action_state.get_current_line(),
		-- 	action_state.get_current_picker(prompt_bufnr)
		-- )
		local cmd = my_split_directions.get_split_direction_cmd()
		-- We have to close the popup, execute the command to split the current
		-- buffer, then resume Telescope and perform the default action on what
		-- we previously had selected.
		require("telescope.actions").close(prompt_bufnr)
		vim.cmd(cmd)
		require("telescope.builtin").resume()
		return action_set.edit(vim.fn.bufnr("%"), "edit")
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
		fzf = {},
	},
})

-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require("telescope").load_extension("ui-select")
require("telescope").load_extension("todo-comments")
require("telescope").load_extension("fzf")
require("telescope").load_extension("file_browser")

local function run_selection(prompt_bufnr, map)
	map("i", "<c-s>", split.buffer_split_in_direction)
	return true
end

local M = {}

function M.my_buffers(opts)
	opts = vim.tbl_extend("keep", { attach_mappings = run_selection }, opts or {})
	require("telescope.builtin").buffers(opts)
end

return M
