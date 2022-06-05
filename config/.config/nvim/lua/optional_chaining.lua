local ts_utils = require("nvim-treesitter.ts_utils")
local treesitter = require("vim.treesitter")

local M = {}

local type_replacement = {
	["arguments"] = [[ :normal! i?.]],
	["."] = [[ :normal! i?]],
	["["] = [[ :normal! i?.]],
}

local function add_optional_chaining_to_node(node)
	for type, cmd in pairs(type_replacement) do
		if node:type() == type then
			ts_utils.goto_node(node, false, true)
			local end_row, end_col = unpack(vim.api.nvim_win_get_cursor(0))

			local text_before = table.concat(
				vim.api.nvim_buf_get_text(0, end_row - 1, end_col - 2, end_row - 1, end_col, {})
			)

			-- Perform command to add optional chaining if we don't have it in
			-- front of this node
			if text_before ~= [[?.]] then
				vim.cmd(cmd)
			end

			break
		end
	end
end

local queries = {
	[[ [ "[" "." ] @char ]],
	[[ (arguments) @args ]],
}

function M.add_optional_chaining()
	local lang = "typescript"
	-- Exit visual mode.
	vim.cmd([[ :normal!  ]])

	local vis_start_row, _ = unpack(vim.api.nvim_buf_get_mark(0, "<"))
	local vis_end_row, _ = unpack(vim.api.nvim_buf_get_mark(0, ">"))

	for i = vis_start_row, vis_end_row do
		vim.api.nvim_win_set_cursor(0, { i, 0 })
		local tree = ts_utils.get_node_at_cursor()

		for _, query in ipairs(queries) do
			local parsed_query = treesitter.parse_query(lang, query)

			-- Lines are 0-indexed.
			for _, node, _ in parsed_query:iter_captures(tree, 0, i - 1, -1) do
				add_optional_chaining_to_node(node)
			end
		end
	end
end

vim.api.nvim_exec([[command! -range OptionalChaining :lua require('optional_chaining').add_optional_chaining()]], false)

return M
