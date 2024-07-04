require("trouble").setup {}

vim.api.nvim_set_keymap("n", "<leader>tl", "<cmd>lua require('trouble').toggle()<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "[t", "<cmd>lua require('trouble').previous({skip_groups=true, jump=true})<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "]t", "<cmd>lua require('trouble').next({skip_groups=true, jump=true})<CR>", { noremap = true })

