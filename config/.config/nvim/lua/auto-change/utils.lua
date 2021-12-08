local M = {}

-- TODO: use nvim_win_set_cursor({window}, {pos}) to set cursor

function M.add_character_at_pos(row, col, char)
    local view = vim.fn.winsaveview()
    vim.cmd('normal ' .. row + 1 .. 'gg')
    vim.cmd('normal ^' .. col .. '|')
    vim.cmd('normal a' .. char)
    vim.fn.winrestview(view)
end

function M.remove_character_at_pos(row, col)
    local view = vim.fn.winsaveview()
    vim.cmd('normal ' .. row + 1 .. 'gg')
    vim.cmd('normal ^' .. col .. '|')
    vim.cmd([[normal lx]])
    vim.fn.winrestview(view)
end

function M.change_character_at_pos(row, col, char)
    local view = vim.fn.winsaveview()
    vim.cmd('normal ' .. row + 1 .. 'gg')
    vim.cmd('normal ^' .. col .. '|l')
    vim.cmd('normal r' .. char)
    vim.fn.winrestview(view)
end

return M
