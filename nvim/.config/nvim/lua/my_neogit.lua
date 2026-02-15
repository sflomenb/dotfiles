local neogit = require("neogit")

vim.keymap.set("n", "<space>gg", "<cmd>Neogit<cr>", { desc = "Open Neogit UI" })

neogit.setup {
  disable_insert_on_commit = true,
  remember_settings = false,
}
