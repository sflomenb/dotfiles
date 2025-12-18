local wk = require("which-key")

wk.add({
	{ "<space>h", group = "Harpoon" },
	{
		"<space>ha",
		function()
			require("harpoon.mark").add_file()
		end,
		desc = "Add file",
	},
	{
		"<space>hc",
		function()
			require("harpoon.mark").clear_all()
		end,
		desc = "Clear all files",
	},
	{
		"<space>hs",
		function()
			require("harpoon.ui").toggle_quick_menu()
		end,
		desc = "Toggle quick menu",
	},
})

local map = {}
for i=1,10 do
	-- Use mod so 0 is 10.
	local mod = i % 10
	table.insert(map, {
		"<M-" .. mod .. ">",
		function()
			require("harpoon.ui").nav_file(i)
		end,
		desc = "Nav file " .. i .. "",
	})
end

wk.add(map)

