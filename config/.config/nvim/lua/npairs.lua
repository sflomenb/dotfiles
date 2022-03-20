local npairs = require("nvim-autopairs")
local Rule = require("nvim-autopairs.rule")

npairs.setup({
	check_ts = true,
})

local ts_conds = require("nvim-autopairs.ts-conds")

npairs.add_rules({
	Rule("{", "}", "python"):with_pair(ts_conds.is_ts_node({ "string" })),
})
