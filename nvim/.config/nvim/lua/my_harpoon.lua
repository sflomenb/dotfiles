local wk = require("which-key")

wk.register({
	name = "Harpoon",
	a = {
		function()
			require("harpoon.mark").add_file()
		end,
		"Add file",
	},
	c = {
		function()
			require("harpoon.mark").clear_all()
		end,
		"Clear all files",
	},
	s = {
		function()
			require("harpoon.ui").toggle_quick_menu()
		end,
		"Toggle quick menu",
	},
}, { prefix = "<space>h" })

wk.register({
	["<C-h>"] = {
		function()
			require("harpoon.ui").nav_file(1)
		end,
		"Nav file 1",
	},
	["<C-j>"] = {
		function()
			require("harpoon.ui").nav_file(2)
		end,
		"Nav file 2",
	},
	["<C-k>"] = {
		function()
			require("harpoon.ui").nav_file(3)
		end,
		"Nav file 3",
	},
	["<C-l>"] = {
		function()
			require("harpoon.ui").nav_file(4)
		end,
		"Nav file 4",
	},
})
