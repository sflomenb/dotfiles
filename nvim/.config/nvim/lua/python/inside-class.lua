local ts_utils = require("nvim-treesitter.ts_utils")
local M = {}

function M.inside_class()
	local lang = vim.bo.ft
	if lang ~= "python" then
		return
	end

	local current_node = ts_utils.get_node_at_cursor()

	if not current_node then
		return false
	end

	if current_node:has_error() then
		current_node = ts_utils.get_previous_node(current_node, true, true)
	end

	if not current_node then
		return false
	end

	if current_node:type() == "class_definition" then
		return true
	end

	while current_node:parent() do
		current_node = current_node:parent()

		if not current_node then
			return
		end

		if current_node:type() == "class_definition" then
			return true
		end
	end

	return false
end

return M
