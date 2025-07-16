local M = {}

function M.determine_indent()
	local res = vim.api.nvim_buf_get_lines(0, 0, 100, false)
	for _, line in pairs(res) do
		local found_space = string.match(line, "^%s+")

		if not found_space then
			goto continue
		end

		if found_space:find("^%t") or not found_space:find("^ ") then
			return 0
		else
			return found_space:len()
		end

		::continue::
	end

	return 0
end

vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function()
		local determined_indent = M.determine_indent()

		if determined_indent == 0 then
			return
		end

		vim.opt_local.ts = determined_indent
		vim.opt_local.sw = determined_indent
		vim.opt_local.sts = determined_indent
	end,
})

return M
