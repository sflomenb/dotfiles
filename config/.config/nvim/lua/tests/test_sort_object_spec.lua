local sort_object = require("sort_object")

local function remove_spaces_in_object(string)
	return string.gsub(string.gsub(string, ",%s*", ", "), "{%s*", "{ ")
end

describe("sort object", function()
	it("should do nothing if the lang is not javascript or typescript", function()
		local buffer_text = [[let foo = { meep: 2, bar: 1 }]]
		-- https://stackoverflow.com/a/40180465/5521899
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, { "let foo = { meep: 2, bar: 1 }" })

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)

		sort_object.sort_object()

		assert.same(vim.api.nvim_buf_get_lines(0, 0, -1, false), { "let foo = { meep: 2, bar: 1 }" })
	end)

	it("does nothing if cursor is not at an object", function()
		local buffer_text = [[let foo = { meep: 2, bar: 1 }]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, { "let foo = { meep: 2, bar: 1 }" })

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 1, 0 })

		sort_object.sort_object()

		assert.same(vim.api.nvim_buf_get_lines(0, 0, -1, false), { "let foo = { meep: 2, bar: 1 }" })
	end)

	it("sorts the object", function()
		local buffer_text = [[let foo = { meep: 2, bar: 1 }]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		assert.same(split_string, { "let foo = { meep: 2, bar: 1 }" })

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 1, 10 })

		sort_object.sort_object()

		assert.same(
			string.gsub(table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n"), "\n", ""),
			"let foo = { bar: 1, meep: 2 }"
		)
	end)

	it("sorts a nested object", function()
		local buffer_text = [[const sortme = {
  bar: 1,
  alala: "wow" as string,
  zeep: {
    q: "epp",
    a: {
      y: "dsfgdsf",
      q: "whwssdfsdee",
      g: 100,
      w: "zoop",
    },
    z: 1,
  },
  meep: 2,
};]]

		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)
		vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 1, 15 })

		sort_object.sort_object()

		assert.are.equal(
			remove_spaces_in_object(
				[[const sortme = { alala: "wow" as string,  bar: 1,  meep: 2,  zeep: { a: { g: 100,      q: "whwssdfsdee",      w: "zoop",      y: "dsfgdsf",    },    q: "epp",    z: 1,  },};]]
			),
			remove_spaces_in_object(
				string.gsub(table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n"), "\n", "")
			)
		)
	end)
end)
