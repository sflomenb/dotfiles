local ts_utils = require('nvim-treesitter.ts_utils')
local utils = require('auto-change.utils')
local M = {}

function M.pythonFString()
    local lang = vim.bo.ft
    if lang == 'python' then
        local bufnr = vim.fn.bufnr('%')
        local current_node = ts_utils.get_node_at_cursor()
        if not current_node then
            print('Unable to get current node')
            return
        end

        local node_text = vim.treesitter.query.get_node_text(current_node, bufnr)

        local isString = current_node:type() == 'string'
        if not isString then
            return
        end
        local hasCurlyBrace = string.find(node_text, '{')
        local isFString = string.find(node_text, '^f')

        if hasCurlyBrace and not isFString then
            local sRow, sCol, _, _ = current_node:range()
            utils.add_character_at_pos(sRow, sCol, 'f')
            vim.cmd([[normal l]])
        elseif not hasCurlyBrace and isFString then
            local sRow, sCol, _, _ = current_node:range()
            utils.remove_character_at_pos(sRow, sCol)
            vim.cmd([[normal h]])
        end
    end
end

return M