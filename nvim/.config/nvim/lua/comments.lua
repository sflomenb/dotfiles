local M = {}

function M.fold_comments(cur_line, comment_strs)
    local is_comment_match = vim.iter(comment_strs):any(function(v, _)
        local escaped = vim.fn.escape(v, '*')
        local pattern = '^\\s*' .. escaped
        local is_match_index = vim.fn.match(cur_line, pattern)
        return is_match_index >= 0
    end)
    if is_comment_match then
        return "1"
    else
        return "0"
    end
end

return M

