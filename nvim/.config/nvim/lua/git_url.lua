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

local function remove_common_prefix(s1, s2)
	local i = 1
	while s1:sub(i, i) == s2:sub(i, i) and i <= #s1 and i <= #s2 do
		i = i + 1
	end
	return s1:sub(i), s2:sub(i)
end

function M.git_url(line1, line2)
	local url = get_url()
	local base_url, repo_name = get_url_info(url)

	local git_main_output = exec("git rev-parse --abbrev-ref origin/HEAD")

	local commit_sha = exec("git log -1 --pretty=%H " .. git_main_output)

	local file_name = vim.fn.expand("%:~:.")

	local repo_root_dir = exec("git rev-parse --show-toplevel")

	local cwd = vim.fn.getcwd(0)

	-- If we are in a subdirectory, get the rest of the path from the repo root.
	if repo_root_dir ~= cwd and cwd:sub(1, #repo_root_dir) == repo_root_dir then
		local full_file_path = vim.fn.expand("%:p")

		local _, file_relative_to_root = remove_common_prefix(repo_root_dir, full_file_path)

		if file_relative_to_root and file_relative_to_root:sub(1, 1) == '/' then
			file_relative_to_root = file_relative_to_root:sub(2)
		end

		file_name = file_relative_to_root
	end

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
