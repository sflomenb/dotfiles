local change_case = require("change_case")

describe("change case", function()
	it("Should convert to camelCase", function()
		local buffer_text = [[my-long-word]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)

		change_case.change_case("camelCase")

		assert.same({ "myLongWord" }, vim.api.nvim_buf_get_lines(0, 0, -1, false))
	end)
	it("Should convert to snake_case", function()
		local buffer_text = [[my-long-word]]
		local split_string = {}
		string.gsub(buffer_text, "([^\n]+)", function(c)
			split_string[#split_string + 1] = c
		end)

		vim.api.nvim_buf_set_lines(0, 0, -1, false, split_string)

		change_case.change_case("snake_case")

		assert.same({ "my_long_word" }, vim.api.nvim_buf_get_lines(0, 0, -1, false))
	end)
end)
