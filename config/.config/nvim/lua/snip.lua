local ls = require("luasnip")
-- some shorthands...
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local fmt = require("luasnip.extras.fmt").fmt
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

ls.filetype_extend("all", { "_" })

-- require("luasnip.loaders.from_snipmate").lazy_load() -- Lazy loading

ls.add_snippets("typescript", {
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
}, { key = "typescript" })

ls.add_snippets("python", {
	s(
		"def",
		fmt(
			[[
              def {}{}{}{}{}
                  {}
    ]],
			{
				i(1),
				t({ "(" }),
				f(function(_, _, _)
					local inside = require("python.inside-class").inside_class()
					if inside then
						return { "self, " }
					else
						return { "" }
					end
				end, {}),
				i(2),
				t({ "):" }),
				i(0),
			}
		)
	),
}, { key = "python" })

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
