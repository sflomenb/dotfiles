local M = {}

function M.inside_class()
	local lang = vim.bo.ft
	if lang ~= "python" then
		return
	end

	local current_node = vim.treesitter.get_node()

	if not current_node then
		return false
	end

	if current_node:has_error() then
		current_node = current_node:parent()
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
