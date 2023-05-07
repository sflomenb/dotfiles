local Job = require("plenary.job")
local a = require("plenary.async")

vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha

local script_path = vim.fn.expand("~/light-dark.scpt")

local function set_color_via_macos_theme(callback)
	local err, _ = a.uv.fs_stat(script_path)
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
			local new_bg = "light"
			for _, v in pairs(results) do
				if string.lower(v) == "light" then
					new_bg = "light"
					vim.g.catppuccin_flavour = "latte"
					break
				elseif string.lower(v) == "dark" then
					new_bg = "dark"
					vim.g.catppuccin_flavour = "macchiato"
					break
				end
			end
			callback(new_bg)
		end,
	}):start()
end

local function should_use_kitty()
	return vim.fn.executable("blahblah") == 1
end

local function set_kitty_theme(new_bg)
	if not should_use_kitty() then
		return
	end

	Job:new({
		-- kitty +kitten themes --reload-in=all Catppuccin-Latte
		command = "kitty",
		args = {
			"+kitten",
			"themes",
			"--reload-in=all",
			[[Catppuccin-]] .. (new_bg == "light" and "Latte" or "Macchiato"),
		},
		on_exit = function(j, return_val)
			if return_val ~= 0 then
				print("on exit")
				print(return_val)
				print("j:result(): " .. (vim.inspect(j:result()) or ""))
			end
		end,
	}):start()
end

vim.api.nvim_create_autocmd("OptionSet", {
	pattern = "background",
	callback = function()
		vim.cmd("Catppuccin " .. (vim.v.option_new == "light" and "latte" or "macchiato"))
	end,
})

local toggle_script_path = vim.fn.expand("~/Library/ApplicationSupport/iTerm2/Scripts/iterm2-light-dark-toggle.py")
local function set_iterm_theme()
	local err, stat = a.uv.fs_stat(toggle_script_path)
	if err or not stat then
		print("err: " .. (vim.inspect(err) or ""))
		return
	end

	Job:new({
		command = "osascript",
		args = {
			"-e",
			[[tell application "iTerm"
        launch API script named "iterm2-light-dark-toggle"
end tell]],
		},
		on_exit = function(j, return_val)
			if return_val ~= 0 then
				print("on exit")
				print(return_val)
				print("j:result(): " .. (vim.inspect(j:result()) or ""))
			end
		end,
	}):start()
end

local function should_use_macos_theme()
	--  https://www.reddit.com/r/neovim/comments/pc7in0/comment/hakuhzn/?utm_source=reddit&utm_medium=web2x&context=3
	local is_macos = vim.loop.os_uname().sysname == "Darwin"

	if not is_macos then
		return false
	end

	local err, stat = a.uv.fs_stat(script_path)
	if err or not stat then
		print("err: " .. (vim.inspect(err) or ""))
		return false
	end

	return true
end

local function set_background_mode(callback)
	local new_bg = "light"
	local ENV_VAR_NAME = "VIM_BACKGROUND"

	if not callback then
		callback = function(bg)
			vim.schedule(function()
				if vim.opt.background:get() ~= bg then
					vim.o.background = bg
					vim.cmd([[colorscheme catppuccin]])
				end
			end)
		end
	end

	local vim_background_env = os.getenv(ENV_VAR_NAME)

	if vim_background_env then
		new_bg = vim_background_env == "light" and "light" or "dark"
		callback(new_bg)
	elseif should_use_macos_theme() then
		local new_callback = function(bg)
			a.run(function()
				set_kitty_theme(bg)
			end)
			callback(bg)
		end
		set_color_via_macos_theme(new_callback)
	else
		-- https://stackoverflow.com/a/68830379/5521899
		local hour = tonumber(os.date("%H"))
		new_bg = (hour > 6 and hour < 18) and "light" or "dark"
		set_kitty_theme(new_bg)
		callback(new_bg)
	end
end

a.run(function()
	set_background_mode(vim.schedule_wrap(function(new_bg)
		local background_table = {
			light = "latte",
			dark = "macchiato",
		}
		require("catppuccin").setup({
			flavour = background_table[new_bg],
			background = background_table,
		})
		vim.cmd.colorscheme("catppuccin")
	end))
end)

local timer = vim.loop.new_timer()
timer:start(
	300,
	300000,
	vim.schedule_wrap(function()
		a.run(function()
			set_background_mode()
		end)
	end)
)

vim.api.nvim_create_autocmd("VimLeavePre", {
	pattern = "*",
	callback = function()
		timer:close()
	end,
})

vim.api.nvim_create_autocmd("FocusGained", {
	pattern = "*",
	callback = function()
		vim.schedule(function()
			a.run(function()
				set_background_mode()
			end)
		end)
	end,
})
