local M = {}

function M.prev_indent_level()
	vim.cmd("norm! m'")
	local row = vim.api.nvim_win_get_cursor(0)[1]
	local current_indent_level = vim.fn.indent(row)
	while current_indent_level <= vim.fn.indent(row) do
		vim.cmd("norm! k")
		row = vim.api.nvim_win_get_cursor(0)[1]
	end
	vim.cmd("norm! ^")
	vim.cmd("norm! m'")
end

vim.api.nvim_exec([[command! PrevIndentLevel :lua require('prev_indent_level').prev_indent_level()]], false)

return M
