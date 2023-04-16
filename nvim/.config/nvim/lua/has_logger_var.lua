local treesitter = require("vim.treesitter")

local query = [[
(assignment
  ((identifier) @name
    (#eq? @name "logger"))
  ) @assign
]]

function _G.has_python_logger_var()
	local lang = "python"

	local tsparser = treesitter.get_parser(0, lang)
	local root = tsparser:parse()[1]:root()

	local parsed_query = treesitter.query.parse_query(lang, query)

	local has_logger = false

	-- Lines are 0-indexed.
	for _, _, _ in parsed_query:iter_captures(root, 0) do
		has_logger = true
		break
	end

	return has_logger
end
