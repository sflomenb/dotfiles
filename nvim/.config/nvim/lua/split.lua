local split_directions = {
	["k"] = "sp",
	["j"] = "bel sp",
	["h"] = "vert sp",
	["l"] = "bel vert sp",
	["J"] = "bot bel sp",
	["K"] = "top abo sp",
	["L"] = "bot bel vert sp",
	["H"] = "top abo vert sp",
	["t"] = "tab sp",
}

local M = {}

function M.get_split_direction_cmd()
	local input = vim.fn.nr2char(vim.fn.getchar())
	return split_directions[input] or nil
end

function M.split_in_direction()
	local cmd = M.get_split_direction_cmd()
	if cmd then
		vim.cmd(cmd)
	end
end

return M
