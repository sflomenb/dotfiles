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

describe("determine_case()", function()
	it("Detects camel case", function()
		assert.are.equal("camelCase", change_case.determine_case("myLongWord"))
	end)
	it("Detects snake case", function()
		assert.are.equal("snake_case", change_case.determine_case("my_long_word"))
	end)
	it("Detects kebab case", function()
		assert.are.equal("kebab-case", change_case.determine_case("my-long-word"))
	end)
	it("Detects pascal case", function()
		assert.are.equal("PascalCase", change_case.determine_case("MyLongWord"))
	end)
	it("Detects upper case snake case", function()
		assert.are.equal("UPPER_CASE_SNAKE_CASE", change_case.determine_case("MY_LONG_WORD"))
	end)
end)

describe("split_word()", function()
	it("Splits camel case", function()
		assert.same({ "my", "Long", "Word" }, change_case.split_word("myLongWord"))
	end)
	it("Splits snake case", function()
		assert.same({ "my", "long", "word" }, change_case.split_word("my_long_word"))
	end)
	it("Splits kebab case", function()
		assert.same({ "my", "long", "word" }, change_case.split_word("my-long-word"))
	end)
	it("Splits pascal case", function()
		assert.same({ "My", "Long", "Word" }, change_case.split_word("MyLongWord"))
	end)
	it("Splits upper case snake case", function()
		assert.same({ "MY", "LONG", "WORD" }, change_case.split_word("MY_LONG_WORD"))
	end)
end)

describe("convert_words_to_case()", function()
	it("Converts to camel case", function()
		assert.are.equal("myLongWord", change_case.convert_words_to_case({ "my", "long", "word" }, "camelCase"))
	end)
	it("Converts to snake case", function()
		assert.are.equal("my_long_word", change_case.convert_words_to_case({ "my", "long", "word" }, "snake_case"))
	end)
	it("Converts to kebab case", function()
		assert.are.equal("my-long-word", change_case.convert_words_to_case({ "my", "long", "word" }, "kebab-case"))
	end)
	it("Converts to pascal case", function()
		assert.are.equal("MyLongWord", change_case.convert_words_to_case({ "my", "long", "word" }, "PascalCase"))
	end)
	it("Converts to upper case snake case", function()
		assert.are.equal(
			"MY_LONG_WORD",
			change_case.convert_words_to_case({ "my", "long", "word" }, "UPPER_CASE_SNAKE_CASE")
		)
	end)
end)
