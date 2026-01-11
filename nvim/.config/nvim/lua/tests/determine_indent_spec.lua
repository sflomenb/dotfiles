local determine_indent = require("determine_indent")

describe("determine_indent", function()
	it("detects 2 space indentation", function()
		vim.api.nvim_buf_set_lines(0, 0, -1, false, { "import foo from bar;", "  console.log('foo');" })
		vim.api.nvim_set_option_value("filetype", "javascript", {buf = 0})
		vim.api.nvim_win_set_cursor(0, { 1, 0 })

		local actual = determine_indent.determine_indent()

		assert.same(2, actual)
	end)

	it("detects 4 space indentation", function()
		vim.api.nvim_buf_set_lines(0, 0, -1, false, { "import foo from bar;", "    console.log('foo');" })
		vim.api.nvim_set_option_value("filetype", "javascript", {buf = 0})
		vim.api.nvim_win_set_cursor(0, { 1, 0 })

		local actual = determine_indent.determine_indent()

		assert.same(4, actual)
	end)

	it("ignores comments without treesitter", function()
		vim.api.nvim_buf_set_lines(0, 0, -1, false, { "import foo from bar;", "/**", " * Some comment.", " */", "    console.log('foo');" })
		vim.api.nvim_set_option_value("filetype", "javascript", {buf = 0})
		vim.cmd([[lua vim.treesitter.stop()]])
		vim.api.nvim_win_set_cursor(0, { 1, 0 })

		local actual = determine_indent.determine_indent()

		assert.same(4, actual)
	end)

	it("ignores comments with treesitter", function()
		vim.api.nvim_buf_set_lines(0, 0, -1, false, { "import foo from bar;", "/**", " * Some comment.", " */", "    console.log('foo');" })
		vim.api.nvim_set_option_value("filetype", "javascript", {buf = 0})
		vim.api.nvim_win_set_cursor(0, { 1, 0 })

		local actual = determine_indent.determine_indent()

		assert.same(4, actual)
	end)

	it("returns 0 when no indentation found", function()
		vim.api.nvim_buf_set_lines(0, 0, -1, false, { "import foo from bar;" })
		vim.api.nvim_set_option_value("filetype", "javascript", {buf = 0})
		vim.api.nvim_win_set_cursor(0, { 1, 0 })

		local actual = determine_indent.determine_indent()

		assert.same(0, actual)
	end)
end)
