local M = {}

function M.determine_indent()
	local res = vim.api.nvim_buf_get_lines(0, 0, 100, false)

	local is_treesitter_enabled = treesitter_enabled()
	if is_treesitter_enabled then
		-- Ensure the buffer is parsed.
		vim.treesitter.get_parser(0):parse()
	end

	for line_num, line in pairs(res) do
		local found_space = string.match(line, "^%s+")

		if not found_space then
			goto continue
		end

		if is_treesitter_enabled then
			-- If is comment, continue.
			-- vim.treesitter.get_node() line nums are 0-indexed.
			local current_node = vim.treesitter.get_node({pos = {line_num-1, 0}})
			if current_node == nil then
				goto continue
			end

			local is_comment = current_node:type() == "comment"
			if is_comment then
				goto continue
			end
		else
			-- Treesitter is disabled, need to check the syntax.
			-- https://vim.fandom.com/wiki/Check_for_comments_independent_of_filetype
			-- If is comment, continue.
			local is_comment = vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.synID(line_num, 1, 0)), "name") == "Comment"
			if is_comment then
				goto continue
			end
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
