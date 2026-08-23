local M = {}

local function contains(str, substr)
    return str:find(substr, 1, true) ~= nil
end

local flag_to_action = {
    s = ">1",
    m = "=",
    e = "<1",
}

function M.fold_comments(cur_line, comment_strs)
    for _, item in ipairs(comment_strs) do
        local flags, str = item[1], item[2]
        -- There will be optional flags or not. If there is just 1 item, there
        -- are no flags.
        if not str then
            str = flags
            flags = nil
        end

        local escaped = vim.fn.escape(str, '*')
        local pattern = '^\\s*' .. escaped
        local is_match = vim.fn.match(cur_line, pattern) >= 0

        if is_match then
            if flags ~= nil then
                for flag, action in pairs(flag_to_action) do
                    if contains(flags, flag) then
                        -- Line matches a flag with special behavior.
                        return action
                    end
                end
            end
            -- Line is a normal comment if flags are not present or no flags
            -- match.
            return "1"
        end
    end
    -- Line is not a comment.
    return "0"
end

return M

