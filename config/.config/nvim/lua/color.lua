local Job = require("plenary.job")
local uv = require("luv")

vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha

local function set_color_via_macos_theme(callback)
	local script_path =
		"/Users/sflomenb/Library/Mobile Documents/com~apple~ScriptEditor2/Documents/Light-Dark Terminal.scpt"

	uv.fs_stat(script_path, function(err, stat)
		if err then
			print("err: " .. (vim.inspect(err) or ""))
			callback()
		end

		local results = {}

		Job:new({
			command = "osascript",
			args = {
				script_path,
			},
			on_stdout = function(_, line)
				table.insert(results, line)
			end,
			on_stderr = function(_, line)
				table.insert(results, line)
			end,
			on_exit = function(_, _)
				for _, v in pairs(results) do
					if string.lower(v) == "light" then
						vim.g.catppuccin_flavour = "latte" -- latte, frappe, macchiato, mocha
						break
					elseif string.lower(v) == "dark" then
						vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha
						break
					end
				end
				callback()
			end,
		}):start()
	end)
end

set_color_via_macos_theme(function()
	vim.schedule(function()
		require("catppuccin").setup()
		vim.cmd([[colorscheme catppuccin]])
	end)
end)

vim.api.nvim_create_autocmd("OptionSet", {
	pattern = "background",
	callback = function()
		vim.cmd("Catppuccin " .. (vim.v.option_new == "light" and "latte" or "macchiato"))
	end,
})

if vim.v.vim_did_enter then
	vim.defer_fn(function()
		vim.fn["SetBackgroundMode"]()
		vim.cmd("Catppuccin " .. (vim.o.background == "light" and "latte" or "macchiato"))
		vim.cmd([[colorscheme catppuccin]])
	end, 500)
end
