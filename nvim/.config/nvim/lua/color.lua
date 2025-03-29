vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha

local background_table = {
	light = "latte",
	dark = "macchiato",
}

local function system_trim_newlines(cmd)
	return string.gsub(vim.fn.system(cmd), "%s+", "")
end

local function set_color_via_time_of_day()
	local hour = tonumber(os.date("%H"))
	return (hour > 6 and hour < 18) and "light" or "dark"
end

local function set_color_via_darkman()
	return system_trim_newlines("darkman get")
end

local function set_color()
	local ENV_VAR_NAME = "VIM_BACKGROUND"

	local vim_background_env = os.getenv(ENV_VAR_NAME)

	if vim_background_env then
		return vim_background_env == "light" and "light" or "dark"
	end

	if system_trim_newlines("uname") == "Linux" and vim.fn.executable("darkman") then
		return set_color_via_darkman()
	end

	-- https://stackoverflow.com/a/68830379/5521899
	return set_color_via_time_of_day()
end

vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		require("catppuccin").setup({
			flavour = background_table[set_color()],
			background = background_table,
			integrations = {
				native_lsp = {
					enabled = true,
				},
				cmp = true,
				gitgutter = true,
				treesitter = true,
			},
		})
		vim.cmd.colorscheme("catppuccin")
	end,
})

local M = {}

function M.update_color(override_bg)
	local bg = override_bg or set_color()
	if vim.opt.background:get() ~= bg then
		vim.o.background = bg
		vim.cmd([[colorscheme catppuccin]])
		vim.cmd("Catppuccin " .. (bg == "light" and "latte" or "macchiato"))
	end
end

return M
