local M = {}

local function trim_string(str)
	return string.gsub(str, "%s+", "")
end

local function exec(cmd)
	return trim_string(vim.fn.system(cmd))
end

local function split_string(str, del)
	local res = {}
	for split_str in string.gmatch(str, "([^" .. del .. "]+)") do
		table.insert(res, split_str)
	end
	return res
end

local function get_url()
	local git_main_output = exec("git remote get-url origin")
	return git_main_output
end

local function get_url_info(url)
	local _, rest = unpack(split_string(url, "\\@"))
	local base_url, repo_name_with_git = unpack(split_string(rest, ":"))
	local repo_name, _ = unpack(split_string(repo_name_with_git, "."))
	return base_url, repo_name
end

function M.git_url(line1, line2)
	local url = get_url()
	local base_url, repo_name = get_url_info(url)

	local git_main_output = exec("git rev-parse --abbrev-ref origin/HEAD")

	local commit_sha = exec("git log -1 --pretty=%H " .. git_main_output)

	local file_name = vim.fn.expand("%:~:.")

	local res = string.format("https://%s/%s/blob/%s/%s", base_url, repo_name, commit_sha, file_name)

	if line1 ~= line2 then
		res = string.format("%s#L%s-L%s", res, line1, line2)
	else
		res = string.format("%s#L%s", res, line1)
	end

	vim.fn.setreg("+", res)
end

vim.api.nvim_create_user_command("GitUrl", function(args)
	M.git_url(args.line1, args.line2)
end, { range = true })

vim.api.nvim_set_keymap("n", "<space>g", "<Cmd>GitUrl<cr>", { noremap = true })
vim.api.nvim_set_keymap("v", "<space>g", ":GitUrl<cr>", { noremap = true })

return M
