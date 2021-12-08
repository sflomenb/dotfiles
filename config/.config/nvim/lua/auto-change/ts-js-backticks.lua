local ts_utils = require('nvim-treesitter.ts_utils')
local utils = require('auto-change.utils')
local M = {}

function M.tsJsBackticks()
    local lang = vim.bo.ft
    if lang == 'javascript' or lang == 'typescript' then
        local bufnr = vim.fn.bufnr('%')
        local current_node = ts_utils.get_node_at_cursor()
        if not current_node then
            print('Unable to get current node')
            return
        end

        local node_text = vim.treesitter.query.get_node_text(current_node, bufnr)

        local isString = current_node:type() == 'string'
        local isStringFragment = current_node:type() == 'string_fragment'

        if not isString and not isStringFragment then
            return
        end

        if isStringFragment then
            current_node = current_node:parent()
        end

        local hasInterpolation = string.find(node_text, '%${')

        if hasInterpolation then
            local sRow, sCol, eRow, eCol = current_node:range()
            utils.change_character_at_pos(sRow, sCol, '`')
            utils.change_character_at_pos(eRow, eCol, '`')
        end
    end
end

return M
