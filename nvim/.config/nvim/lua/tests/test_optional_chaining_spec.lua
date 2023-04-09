local optional_chaining = require("optional_chaining")

describe("optional chaining", function()
	it("adds optional chaining for the whole line", function()
		local buffer_text = [[const foo = whee.Foo['Whee Saw'].lala.fakeMethod();]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, { "const foo = whee.Foo['Whee Saw'].lala.fakeMethod();" })

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 1, 0 })
		vim.cmd([[ normal! V ]])

		optional_chaining.add_optional_chaining()

		assert.same(
			"const foo = whee?.Foo?.['Whee Saw']?.lala?.fakeMethod?.();",
			string.gsub(table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n"), "\n", "")
		)
	end)

	it("adds optional chaining for the whole line when there are multiple lines", function()
		local buffer_text = [[const bar = 'hello world!';
const foo = whee.Foo['Whee Saw'].lala.fakeMethod();]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(
			split_string,
			{ "const bar = 'hello world!';", "const foo = whee.Foo['Whee Saw'].lala.fakeMethod();" }
		)

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 2, 0 })
		vim.cmd([[ normal! V ]])

		optional_chaining.add_optional_chaining()

		assert.same(
			{ "const bar = 'hello world!';", "const foo = whee?.Foo?.['Whee Saw']?.lala?.fakeMethod?.();" },
			vim.api.nvim_buf_get_lines(0, 0, -1, false)
		)
	end)

	it("adds optional chaining for the whole line when there are multiple lines surrounding", function()
		local buffer_text = [[const bar = 'hello world!';
const foo = whee.Foo['Whee Saw'].lala.fakeMethod();
const meep = wow.zip;]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, {
			"const bar = 'hello world!';",
			"const foo = whee.Foo['Whee Saw'].lala.fakeMethod();",
			"const meep = wow.zip;",
		})

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 2, 0 })
		vim.cmd([[ normal! V ]])

		optional_chaining.add_optional_chaining()

		assert.same({
			"const bar = 'hello world!';",
			"const foo = whee?.Foo?.['Whee Saw']?.lala?.fakeMethod?.();",
			"const meep = wow.zip;",
		}, vim.api.nvim_buf_get_lines(0, 0, -1, false))
	end)

	it("adds optional chaining for multiple lines", function()
		local buffer_text = [[const bar = 'hello world!';
const foo = whee.Foo['Whee Saw'].lala.fakeMethod();
const meep = wow.zip;]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, {
			"const bar = 'hello world!';",
			"const foo = whee.Foo['Whee Saw'].lala.fakeMethod();",
			"const meep = wow.zip;",
		})

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 2, 0 })
		vim.cmd([[ normal! Vj ]])

		optional_chaining.add_optional_chaining()

		assert.same({
			"const bar = 'hello world!';",
			"const foo = whee?.Foo?.['Whee Saw']?.lala?.fakeMethod?.();",
			"const meep = wow?.zip;",
		}, vim.api.nvim_buf_get_lines(0, 0, -1, false))
	end)

	it("adds optional chaining for part of a line", function()
		local buffer_text = [[const bar = 'hello world!';
const foo = whee.Foo['Whee Saw'].lala.fakeMethod();
const meep = wow.zip;]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, {
			"const bar = 'hello world!';",
			"const foo = whee.Foo['Whee Saw'].lala.fakeMethod();",
			"const meep = wow.zip;",
		})

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 2, 0 })
		vim.cmd([[ normal! ^fwvf[ ]])

		optional_chaining.add_optional_chaining()

		assert.same({
			"const bar = 'hello world!';",
			"const foo = whee?.Foo?.['Whee Saw'].lala.fakeMethod();",
			"const meep = wow.zip;",
		}, vim.api.nvim_buf_get_lines(0, 0, -1, false))
	end)

	it("does not add to if statement parenthesis", function()
		local buffer_text = [[if(whee.Foo['Whee Saw'].lala.fakeMethod()) {}]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, {
			"if(whee.Foo['Whee Saw'].lala.fakeMethod()) {}",
		})

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 1, 0 })
		vim.cmd([[ normal! V ]])

		optional_chaining.add_optional_chaining()

		assert.same({
			"if(whee?.Foo?.['Whee Saw']?.lala?.fakeMethod?.()) {}",
		}, vim.api.nvim_buf_get_lines(0, 0, -1, false))
	end)
end)
