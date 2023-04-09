local M = {}

function M.determine_case(word)
	if vim.regex([[^[A-Z]\+\(_[A-Z]\+\)*$]]):match_str(word) then
		return "UPPER_CASE_SNAKE_CASE"
	elseif word:find("-", 1, false) then
		return "kebab-case"
	elseif word:find("_", 1, false) then
		return "snake_case"
	elseif word:find("^[A-Z]") then
		return "PascalCase"
	else
		return "camelCase"
	end
end

function M.split_word(word)
	local case = M.determine_case(word)
	if case == "UPPER_CASE_SNAKE_CASE" then
		return vim.fn.split(word, "_")
	elseif case == "snake_case" then
		return vim.fn.split(word, "_")
	elseif case == "kebab-case" then
		return vim.fn.split(word, "-")
	elseif case == "PascalCase" then
		return vim.fn.split(word, "\\ze[A-Z]", 0)
	elseif case == "camelCase" then
		return vim.fn.split(word, "\\ze[A-Z]", 0)
	end
end

local function map(tbl, f)
	local ret = {}
	for _, v in ipairs(tbl) do
		table.insert(ret, f(v))
	end
	return ret
end

local function convert_to_pascal_case(word)
	return vim.fn.substitute(word, [[\v^(\a)(\a*)]], [[\u\1\L\2]], "")
end

function M.convert_words_to_case(words, case)
	if case == "UPPER_CASE_SNAKE_CASE" then
		return table.concat(map(words, string.upper), "_")
	elseif case == "snake_case" then
		return table.concat(map(words, string.lower), "_")
	elseif case == "kebab-case" then
		return table.concat(map(words, string.lower), "-")
	elseif case == "PascalCase" then
		return table.concat(map(words, convert_to_pascal_case), "")
	elseif case == "camelCase" then
		local lowercase_first_word = string.lower(words[1])
		table.remove(words, 1)
		return lowercase_first_word .. table.concat(map(words, convert_to_pascal_case), "")
	end
end

local function split_and_change_word(word, case)
	local split_words = M.split_word(word)
	local new_word = M.convert_words_to_case(split_words, case)
	vim.fn.setreg("z", new_word)
	vim.cmd('norm! viw"zp')
	-- Restore iskeyword.
	vim.cmd("set iskeyword<")
end

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
