vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha

local background_table = {
	light = "latte",
	dark = "macchiato",
}

local function set_color_via_time_of_day()
	local hour = tonumber(os.date("%H"))
	return (hour > 6 and hour < 18) and "light" or "dark"
end

local function set_color()
	local ENV_VAR_NAME = "VIM_BACKGROUND"

	local vim_background_env = os.getenv(ENV_VAR_NAME)

	if vim_background_env then
		return vim_background_env == "light" and "light" or "dark"
	else
		-- https://stackoverflow.com/a/68830379/5521899
		return set_color_via_time_of_day()
	end
end

vim.api.nvim_create_autocmd("OptionSet", {
	pattern = "background",
	callback = function()
		vim.cmd("Catppuccin " .. (vim.v.option_new == "light" and "latte" or "macchiato"))
	end,
})

require("catppuccin").setup({
	flavour = background_table[set_color()],
	background = background_table,
})
vim.cmd.colorscheme("catppuccin")

local M = {}

function M.update_color()
	local bg = set_color()
	if vim.opt.background:get() ~= bg then
		vim.o.background = bg
		vim.cmd([[colorscheme catppuccin]])
	end
end

return M
