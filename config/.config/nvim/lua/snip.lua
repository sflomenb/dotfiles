local ls = require("luasnip")
-- some shorthands...
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local c = ls.choice_node
local d = ls.dynamic_node
local types = require("luasnip.util.types")

-- If you're reading this file for the first time, best skip to around line 190
-- where the actual snippet-definitions start.

-- Every unspecified option will be set to the default.
ls.config.set_config({
	history = true,
	-- Update more often, :h events for more info.
	updateevents = "TextChanged,TextChangedI",
	ext_opts = {
		[types.choiceNode] = {
			active = {
				virt_text = { { "choiceNode", "Comment" } },
			},
		},
	},
	-- treesitter-hl has 100, use something higher (default is 200).
	ext_base_prio = 300,
	-- minimal increase in priority.
	ext_prio_increase = 1,
	enable_autosnippets = true,
})

-- 'recursive' dynamic snippet. Expands to some text followed by itself.
local rec_ls
rec_ls = function()
	return sn(
		nil,
		c(1, {
			-- Order is important, sn(...) first would cause infinite loop of expansion.
			t(""),
			sn(nil, { t({ "", "\t\\item " }), i(1), d(2, rec_ls, {}) }),
		})
	)
end

--[[
-- Beside defining your own snippets you can also load snippets from "vscode-like" packages
-- that expose snippets in json files, for example <https://github.com/rafamadriz/friendly-snippets>.
-- Mind that this will extend  `ls.snippets` so you need to do it after your own snippets or you
-- will need to extend the table yourself instead of setting a new one.
]]

-- require("luasnip.loaders.from_vscode").load({ include = { "python", "rust", "typescript" } }) -- Load only python snippets
-- -- The directories will have to be structured like eg. <https://github.com/rafamadriz/friendly-snippets> (include
-- -- a similar `package.json`)
-- require("luasnip.loaders.from_vscode").load({ paths = { "./my-snippets" } }) -- Load snippets from my-snippets folder

-- You can also use lazy loading so you only get in memory snippets of languages you use
require("luasnip.loaders.from_vscode").lazy_load() -- You can pass { paths = "./my-snippets/"} as well

-- You can also use snippets in snipmate format, for example <https://github.com/honza/vim-snippets>.
-- The usage is similar to vscode.

-- One peculiarity of honza/vim-snippets is that the file with the global snippets is _.snippets, so global snippets
-- are stored in `ls.snippets._`.
-- We need to tell luasnip that "_" contains global snippets:
ls.filetype_extend("all", { "_" })

require("luasnip.loaders.from_snipmate").load({ include = { "c" } }) -- Load only python snippets

require("luasnip.loaders.from_snipmate").load({ path = { "./my-snippets" } }) -- Load snippets from my-snippets folder
-- If path is not specified, luasnip will look for the `snippets` directory in rtp (for custom-snippet probably
-- `~/.config/nvim/snippets`).

require("luasnip.loaders.from_snipmate").lazy_load() -- Lazy loading

ls.snippets = {
	typescript = {
		s("desc", {
			t({ "describe('" }),
			i(1),
			t({ "', async function() {" }),
			t({ "", "  " }),
			i(0),
			t({ "", "});" }),
		}),
		s("it", {
			c(1, {
				t("it"),
			}, {}),
			t({ "('" }),
			i(2),
			t({ "', async function() {" }),
			t({ "", "  " }),
			i(0),
			t({ "", "});" }),
		}),
		s("inittest", {
			t({ "import nock from 'nock';", "", "" }),
			sn(1, {
				t({ "describe('" }),
				i(1),
				t({
					"', async function() {",
					"  before(async function() {",
					"    nock.disableNetConnect();",
					"});",
					"",
					"  after(async function() {",
					"    nock.enableNetConnect();",
					"});",
					"",
				}),
				t({ "", "  " }),
				i(2),
				t({ "", "});" }),
			}),
		}),
	},
}

-- autotriggered snippets have to be defined in a separate table, luasnip.autosnippets.
ls.autosnippets = {
	all = {
		s("autotrigger", {
			t("autosnippet"),
		}),
	},
}

vim.api.nvim_set_keymap("i", "<C-E>", "<Plug>luasnip-next-choice", {})
vim.api.nvim_set_keymap("s", "<C-E>", "<Plug>luasnip-next-choice", {})
