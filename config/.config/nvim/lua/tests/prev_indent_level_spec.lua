local prev_indent_level = require("prev_indent_level")

describe("prev indent level", function()
	it("should go to the top of the object", function()
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
		-- vim.api.nvim_buf_set_option(0, "filetype", "typescript")
		vim.api.nvim_win_set_cursor(0, { 7, 19 })

		prev_indent_level.prev_indent_level()

		assert.same({ 6, 4 }, vim.api.nvim_win_get_cursor(0))
	end)
end)
