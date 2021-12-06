local function set_keymap(...) vim.api.nvim_set_keymap(...) end

local opts = { noremap=true, silent=true }

set_keymap('n', '<Leader>f', "<cmd>lua require('telescope.builtin').find_files({ hidden = true })<cr>", opts)
set_keymap('n', '<Leader>g', "<cmd>lua require('telescope.builtin').git_files()<cr>", opts)
set_keymap('n', '<Leader>b', "<cmd>lua require('telescope.builtin').buffers()<cr>", opts)

