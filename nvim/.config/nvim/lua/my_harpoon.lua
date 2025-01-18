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

wk.add({
	{
		"<C-h>",
		function()
			require("harpoon.ui").nav_file(1)
		end,
		desc = "Nav file 1",
	},
	{
		"<C-j>",
		function()
			require("harpoon.ui").nav_file(2)
		end,
		desc = "Nav file 2",
	},
	{
		"<C-k>",
		function()
			require("harpoon.ui").nav_file(3)
		end,
		desc = "Nav file 3",
	},
	{
		"<C-l>",
		function()
			require("harpoon.ui").nav_file(4)
		end,
		desc = "Nav file 4",
	},
})

