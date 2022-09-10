vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha

require("catppuccin").setup()

vim.cmd([[colorscheme catppuccin]])

vim.api.nvim_create_autocmd("OptionSet", {
	pattern = "background",
	callback = function()
		vim.cmd("Catppuccin " .. (vim.v.option_new == "light" and "latte" or "macchiato"))
	end,
})

if vim.v.vim_did_enter then
	vim.fn["SetBackgroundMode"]()
	vim.cmd("Catppuccin " .. (vim.o.background == "light" and "latte" or "macchiato"))
	vim.cmd([[colorscheme catppuccin]])
end
