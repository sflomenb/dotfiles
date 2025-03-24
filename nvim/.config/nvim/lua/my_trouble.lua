require("trouble").setup({})

vim.keymap.set("n", "<leader>tl", "<cmd>lua require('trouble').toggle('diagnostics')<CR>", { noremap = true })
vim.keymap.set("n", "[t", "<cmd>lua require('trouble').prev({skip_groups=true, jump=true})<CR>", { noremap = true })
vim.keymap.set("n", "]t", "<cmd>lua require('trouble').next({skip_groups=true, jump=true})<CR>", { noremap = true })

