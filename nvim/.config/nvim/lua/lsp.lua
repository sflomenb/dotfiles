-- To debug, uncomment the below line:
-- vim.lsp.set_log_level('debug')
local nvim_lsp = require("lspconfig")
local null_ls = require("null-ls")
local command_resolver = require("null-ls.helpers.command_resolver")
local luasnip = require("luasnip")
local lsp_util = vim.lsp.util
local lsp_status = require("lsp-status")
local lsp_inlay_hints = require("lsp-inlayhints")

lsp_status.register_progress()
lsp_status.config({
	indicator_errors = "E",
	indicator_warnings = "W",
	indicator_info = "i",
	indicator_hint = "?",
	indicator_ok = "Ok",
	kind_labels = {},
	current_function = false,
	indicator_separator = "|",
})

local function buf_set_keymap(...)
	vim.api.nvim_buf_set_keymap(...)
end

local function buf_set_option(...)
	vim.api.nvim_buf_set_option(...)
end

local default_on_attach = function(client, bufnr)
	-- Enable completion triggered by <c-x><c-o>
	buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Mappings.
	local opts = { noremap = true, silent = true }

	-- See `:help vim.lsp.*` for documentation on any of the below functions
	buf_set_keymap(bufnr, "n", "gd", "<cmd>lua require('telescope.builtin').lsp_definitions()<cr>", opts)
	buf_set_keymap(bufnr, "n", "gD", "<cmd>lua require('lsp').my_goto_definition()<cr>", opts)
	buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>k", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
	buf_set_keymap(
		bufnr,
		"n",
		"<space>wl",
		"<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
		opts
	)
	buf_set_keymap(bufnr, "n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
	buf_set_keymap(bufnr, "n", "gr", "<cmd>lua require('telescope.builtin').lsp_references()<cr>", opts)
	buf_set_keymap(bufnr, "n", "<space>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
	buf_set_keymap(bufnr, "n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
	buf_set_keymap(bufnr, "n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>f", "<cmd>lua vim.lsp.buf.format()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>i", "<cmd>lua vim.lsp.buf.incoming_calls()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>o", "<cmd>lua vim.lsp.buf.outgoing_calls()<CR>", opts)
	buf_set_keymap(bufnr, "n", "<space>s", "<cmd>lua require('telescope.builtin').lsp_document_symbols()<cr>", opts)
	buf_set_keymap(bufnr, "n", "<space>a", "v:lua.perform_code_action()", { noremap = true, expr = true })
	buf_set_keymap(bufnr, "x", "<space>a", "v:lua.perform_code_action()", { noremap = true, expr = true })
	buf_set_keymap(bufnr, "n", "<space>al", "v:lua.perform_code_action() .. '_'", { noremap = true, expr = true })

	if client.server_capabilities.documentHighlightProvider then
		vim.api.nvim_exec(
			[[
			  augroup lsp_document_highlight
				autocmd! * <buffer>
				autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
				autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
			  augroup END
			]],
			false
		)
	end

	if client.server_capabilities.codeActionProvider then
		vim.cmd([[ autocmd CursorHold,CursorHoldI <buffer> lua require('lsp').code_action_listener() ]])
	end

	-- https://github.com/jose-elias-alvarez/null-ls.nvim/issues/436#issuecomment-993855498
	local name = client.name
	local is_null = name == "null-ls"
	if not is_null then
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end

	lsp_status.on_attach(client)

	lsp_inlay_hints.setup()

	lsp_inlay_hints.on_attach(client, bufnr, false)
end

local on_attach = function(client, bufnr)
	default_on_attach(client, bufnr)
end

-- Set default client capabilities plus window/workDoneProgress
local capabilities = require("cmp_nvim_lsp").default_capabilities()
capabilities = vim.tbl_extend("keep", capabilities or {}, lsp_status.capabilities)

local servers_with_default_config = { "pyright", "eslint", "terraform_lsp", "ansiblels" }

local myopts = {
	on_attach = on_attach,
	flags = {
		debounce_text_changes = 150,
	},
	capabilities = capabilities,
}
for _, lsp in ipairs(servers_with_default_config) do
	nvim_lsp[lsp].setup(myopts)
end

nvim_lsp.gopls.setup(vim.tbl_extend("keep", {
	init_options = {
		hints = {
			assignVariableTypes = true,
			compositeLiteralFields = true,
			constantValues = true,
			functionTypeParameters = true,
			parameterNames = true,
			rangeVariableTypes = true,
		},
	},
}, myopts))

local extension_path = vim.env.HOME .. "/Downloads/debug-extension-2/extension/"
local rust_dap_config

if vim.fn.filereadable(extension_path) then
	local codelldb_path = extension_path .. "adapter/codelldb"
	local liblldb_path = extension_path .. "lldb/lib/liblldb.dylib"

	if vim.fn.filereadable(codelldb_path) and vim.fn.filereadable(liblldb_path) then
		rust_dap_config = {
			adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
		}
	else
		rust_dap_config = {}
	end
end

require("rust-tools").setup({
	server = {
		on_attach = function(client, bufnr)
			default_on_attach(client, bufnr)

			client.server_capabilities.documentFormattingProvider = false
			client.server_capabilities.documentRangeFormattingProvider = false
		end,
		flags = {
			debounce_text_changes = 150,
		},
		capabilities = capabilities,
	},
	tools = {
		runnables = {
			use_telescope = true,
		},
		inlay_hints = {
			-- disabled due to lsp-inlayhints
			auto = false,
		},
	},
	dap = rust_dap_config,
})

nvim_lsp.tsserver.setup({
	-- Needed for inlayHints. Merge this table with your settings or copy
	-- it from the source if you want to add your own init_options.
	init_options = require("nvim-lsp-ts-utils").init_options,
	--
	on_attach = function(client, bufnr)
		default_on_attach(client, bufnr)
		local ts_utils = require("nvim-lsp-ts-utils")

		-- defaults
		ts_utils.setup({
			debug = true,
			disable_commands = false,
			enable_import_on_completion = true,

			-- import all
			import_all_timeout = 5000, -- ms
			-- lower numbers = higher priority
			import_all_priorities = {
				same_file = 1, -- add to existing import statement
				local_files = 2, -- git files or files with relative path markers
				buffer_content = 3, -- loaded buffer content
				buffers = 4, -- loaded buffer names
			},
			import_all_scan_buffers = 100,
			import_all_select_source = false,

			-- filter diagnostics
			filter_out_diagnostics_by_severity = {},
			filter_out_diagnostics_by_code = {},

			-- inlay hints
			auto_inlay_hints = false,

			-- update imports on file move
			update_imports_on_move = false,
			require_confirmation_on_move = false,
			watch_dir = nil,
		})

		-- required to fix code action ranges and filter diagnostics
		ts_utils.setup_client(client)

		-- no default maps, so you may want to define some here
		local opts = { noremap = true, silent = true }
		vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>o", ":TSLspOrganize<CR>", opts)
		vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>r", ":TSLspRenameFile<CR>", opts)
		vim.api.nvim_buf_set_keymap(bufnr, "n", "<space>i", ":TSLspImportAll<CR>", opts)

		client.server_capabilities.document_formatting = false
		client.server_capabilities.document_range_formatting = false
	end,
	flags = {
		debounce_text_changes = 150,
	},
})

-- Set completeopt to have a better completion experience
vim.o.completeopt = "menuone,noselect"

local has_words_before = function()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- nvim-cmp setup
local cmp = require("cmp")
cmp.setup({
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	mapping = {
		["<C-p>"] = cmp.mapping.select_prev_item(),
		["<C-n>"] = cmp.mapping.select_next_item(),
		["<C-d>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.close(),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			elseif has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end, { "i", "s" }),

		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { "i", "s" }),

		["<F1>9"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<F1>10"] = cmp.mapping(function()
			if luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			end
		end, { "i", "s" }),
	},
	sources = {
		{ name = "path" },
		{ name = "buffer" },
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
		{ name = "crates" },
	},
})

nvim_lsp.lua_ls.setup({
	on_attach = on_attach,
	flags = {
		debounce_text_changes = 150,
	},
	capabilities = capabilities,
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
				-- Setup your lua path
				path = runtime_path,
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
				checkThirdParty = false,
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
			hint = {
				enable = true,
				setType = true,
			},
			format = {
				enable = false,
			},
		},
	},
})

null_ls.setup({
	on_attach = function(client, bufnr)
		default_on_attach(client, bufnr)
		if client.server_capabilities.documentFormattingProvider then
			vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.format({bufnr=bufnr, async=false})")
		end
	end,
	sources = {
		null_ls.builtins.formatting.prettier.with({
			dynamic_command = command_resolver.from_yarn_pnp(),
			conditions = function(utils)
				return utils.root_has_file({ ".pnp.cjs" })
			end,
		}),
		null_ls.builtins.formatting.prettier.with({
			conditions = function(utils)
				return not utils.root_has_file({ ".pnp.cjs" })
			end,
		}),
		null_ls.builtins.formatting.black,
		null_ls.builtins.formatting.gofmt,
		null_ls.builtins.formatting.goimports,
		null_ls.builtins.formatting.rustfmt,
		null_ls.builtins.formatting.terraform_fmt,
		null_ls.builtins.formatting.stylua,
		null_ls.builtins.formatting.jq,
		null_ls.builtins.formatting.isort,

		null_ls.builtins.diagnostics.golangci_lint,
		null_ls.builtins.diagnostics.shellcheck,
		null_ls.builtins.diagnostics.mypy,

		null_ls.builtins.code_actions.shellcheck,
	},
})

local M = {}

vim.fn.sign_define("code_action", { text = "A" })
ACTION_SIGN_ID = 5

-- https://github.com/neovim/nvim-lspconfig/wiki/Code-Actions
function M.code_action_listener()
	local timer = vim.loop.new_timer()
	timer:start(
		500,
		0,
		vim.schedule_wrap(function()
			vim.fn.sign_unplace("my_signs", { id = ACTION_SIGN_ID, buffer = vim.fn.bufname("%") })
			local context = { diagnostics = vim.lsp.diagnostic.get_line_diagnostics() }
			local params = lsp_util.make_range_params()
			params.context = context
			vim.lsp.buf_request(0, "textDocument/codeAction", params, function(err, result, _)
				if err then
					return
				end
				if result and next(result) then
					vim.fn.sign_place(
						ACTION_SIGN_ID,
						"my_signs",
						"code_action",
						vim.fn.bufname("%"),
						{ lnum = vim.fn.line(".") }
					)
				end
			end)
		end)
	)
end

_G.perform_code_action = function(type)
	if not type or type == "" then
		vim.o.operatorfunc = "v:lua.perform_code_action"
		return "g@"
	end

	local current_mode = vim.fn.mode()
	local last_visual_mode = ""
	local is_visual_mode = current_mode == "v" or current_mode == "V" or current_mode == ""

	if not is_visual_mode then
		last_visual_mode = vim.fn.visualmode()
	end

	local selection = vim.o.selection
	vim.o.selection = "inclusive"
	local reg = vim.fn.getreginfo('"')
	local clipboard = vim.o.clipboard
	vim.o.clipboard = ""
	local visual_marks = {
		vim.api.nvim_buf_get_mark(0, "<"),
		vim.api.nvim_buf_get_mark(0, ">"),
	}

	-- Grab text so we can properly call the function.
	if type == "line" then
		vim.cmd([[ normal! '[V']y ]])
	elseif type == "char" then
		vim.cmd([[ normal! `[v`]y ]])
	end

	-- Save positions to use in function later.
	local start_pos = vim.api.nvim_buf_get_mark(0, "[")
	local end_pos = vim.api.nvim_buf_get_mark(0, "]")

	-- Restore all the saved values before we call the function since we cannot
	-- do this after.
	vim.o.selection = selection
	vim.fn.setreg('"', reg)
	vim.o.clipboard = clipboard
	-- Restore last visual mode type if we weren't already in visual mode by
	-- switching to the normal mode temporarily.
	-- NOTE: This must come before we restore the visual marks.
	if last_visual_mode then
		vim.cmd("normal! " .. last_visual_mode .. "")
	end
	vim.api.nvim_buf_set_mark(0, "<", visual_marks[1][1], visual_marks[1][2], {})
	vim.api.nvim_buf_set_mark(0, ">", visual_marks[2][1], visual_marks[2][2], {})

	-- Actually call the function with saved positions.
	require("lsp").range_code_actions({
		start_pos = start_pos,
		end_pos = end_pos,
	})
end

function M.range_code_actions(opts)
	local start_pos = opts.start_pos or vim.api.nvim_buf_get_mark(0, "<")
	local end_pos = opts.end_pos or vim.api.nvim_buf_get_mark(0, ">")
	vim.lsp.buf.code_action({ range = { start = start_pos, ["end"] = end_pos } })
end

function M.my_goto_definition(opts)
	require("split").split_in_direction()
	require("telescope.builtin").lsp_definitions(opts or {})
end

return M
