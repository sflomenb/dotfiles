local M = {}

local function split_and_change_word(word, case)
	local split_words = vim.fn.SplitWord(word)
	local new_word = vim.fn.ConvertWordsToCase(split_words, case)
	vim.fn.setreg("z", new_word)
	vim.cmd('norm! viw"zp')
	-- Restore iskeyword.
	vim.cmd("set iskeyword<")
end

-- TODO: Convert other functions used here to lua.

function M.change_case(case)
	-- Convert to confirm format with each word prefixed with & and folowed by \n
	vim.opt_local.iskeyword:append("-")

	local word = vim.fn.expand("<cword>")
	local cases = { "camelCase", "snake_case", "kebab-case", "PascalCase", "UPPER_CASE_SNAKE_CASE" }

	if not case then
		vim.ui.select(cases, { prompt = "Choose case:" }, function(selected_case)
			split_and_change_word(word, selected_case)
		end)
		return
	end

	split_and_change_word(word, case)
end

vim.api.nvim_exec([[command! ChangeCase :lua require('change_case').change_case()]], false)

return M
