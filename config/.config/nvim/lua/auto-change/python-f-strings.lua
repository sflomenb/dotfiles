local ts_utils = require("nvim-treesitter.ts_utils")
local utils = require("auto-change.utils")
local M = {}

local function get_node_and_text(bufnr)
	local current_node = ts_utils.get_node_at_cursor()
	if not current_node then
		print("Unable to get current node")
		return {}
	end
	if current_node:has_error() then
		return {}
	end

	local is_string = current_node:type() == "string"
	if not is_string then
		return {}
	end

	local node_text = vim.treesitter.query.get_node_text(current_node, bufnr)
	if not node_text then
		return {}
	end

	return { current_node, node_text }
end

function M.pythonFString()
	vim.schedule_wrap(function()
		local lang = vim.bo.ft
		if lang ~= "python" then
			return
		end

		local bufnr = vim.fn.bufnr("%")

		local current_node, node_text = unpack(get_node_and_text(bufnr))

		if not current_node or not node_text then
			return
		end

		local has_open_curly_brace = string.find(node_text, "{")
		local is_f_string = string.find(node_text, "^f")

		if has_open_curly_brace and not is_f_string then
			current_node, node_text = unpack(get_node_and_text(bufnr))

			if not current_node or not node_text then
				return
			end
			local srow, scol, _, _ = current_node:range()
			vim.cmd("silent! undojoin")
			utils.add_character_at_pos(srow, scol, "f")
			vim.cmd([[normal l]])
		elseif not has_open_curly_brace and is_f_string then
			current_node, node_text = unpack(get_node_and_text(bufnr))

			if not current_node or not node_text then
				return
			end
			local srow, scol, _, _ = current_node:range()
			vim.cmd("silent! undojoin")
			utils.remove_character_at_pos(srow, scol)
			vim.cmd([[normal h]])
		end
	end)()
end

return M
