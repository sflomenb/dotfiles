local M = {}

function M.add_character_at_pos(row, col, char)
    local view = vim.fn.winsaveview()
    vim.api.nvim_win_set_cursor(0, {row+1, col})
    vim.cmd('normal i' .. char)
    vim.fn.winrestview(view)
end

function M.remove_character_at_pos(row, col)
    local view = vim.fn.winsaveview()
    vim.api.nvim_win_set_cursor(0, {row+1, col})
    vim.cmd([[normal x]])
    vim.fn.winrestview(view)
end

function M.change_character_at_pos(row, col, char)
    local view = vim.fn.winsaveview()
    vim.api.nvim_win_set_cursor(0, {row+1, col})
    vim.cmd('normal r' .. char)
    vim.fn.winrestview(view)
end

return M
