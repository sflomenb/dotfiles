vim.g.netrw_liststyle = 0
vim.g.netrw_keepdir = 0
vim.g.netrw_banner = 0
vim.g.netrw_altv = 1
vim.g.newtr_winsize = 25

vim.g.start_dir = ""

vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		vim.g.start_dir = vim.fn.getcwd()
	end,
})

vim.api.nvim_create_autocmd("BufLeave", {
	callback = function()
		if vim.o.filetype == "netrw" then
			print([[ cd  ]] .. vim.g.start_dir)
			vim.cmd([[ cd  ]] .. vim.g.start_dir)
		end
	end,
})
