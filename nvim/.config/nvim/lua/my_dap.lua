local dap = require("dap")

dap.adapters.lldb = {
	type = "executable",
	command = "",
	name = "lldb",
}

require("dapui").setup()

require("which-key").add({
	{ "<space>d", group = "Debug" },
	{ "<space>dI", "<cmd>lua require('dapui').toggle({reset=true})<CR>", desc = "Toggle and reset layout" },
	{ "<space>db", group = "Breakpoints" },
	{
		"<space>dbc",
		"<cmd>lua require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>",
		desc = "Breakpoint Condition",
	},
	{ "<space>dbd", "<cmd>lua require('dap').clear_breakpoints()<CR>", desc = "Delete" },
	{
		"<space>dbl",
		"<cmd>lua require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>",
		desc = "Log Point Message",
	},
	{ "<space>dbt", "<cmd>lua require('dap').toggle_breakpoint()<CR>", desc = "Toggle" },
	{ "<space>dc", "<cmd>lua require('dap.ui.variables').scopes()<CR>", desc = "Scopes" },
	{ "<space>dh", group = "Hover" },
	{ "<space>dhh", "<cmd>lua require('dap.ui.widgets').hover()<CR>", desc = "Hover" },
	{ "<space>dhv", "<cmd>lua require('dap.ui.widgets').visual_hover()<CR>", desc = "Visual Hover" },
	{ "<space>di", "<cmd>lua require('dapui').toggle()<CR>", desc = "Toggle" },
	{ "<space>dr", group = "Repl" },
	{ "<space>drl", "<cmd>lua require('dap').repl.run_last()<CR>", desc = "Run Last" },
	{ "<space>dro", "<cmd>lua require('dap').repl.open()<CR>", desc = "Open" },
	{ "<space>ds", group = "Step" },
	{ "<space>dsc", "<cmd>lua require('dap').continue()<CR>", desc = "Continue" },
	{ "<space>dsi", "<cmd>lua require('dap').step_into()<CR>", desc = "Step Into" },
	{ "<space>dso", "<cmd>lua require('dap').step_out()<CR>", desc = "Step Out" },
	{ "<space>dsu", "<cmd>lua require('dap').run_to_cursor()<CR>", desc = "Run To Cursor" },
	{ "<space>dsv", "<cmd>lua require('dap').step_over()<CR>", desc = "Step Over" },
	{ "<space>dt", "<cmd>lua require('dap').terminate()<CR>", desc = "Terminate" },
	{ "<space>du", group = "UI" },
	{
		"<space>duf",
		"local widgets=require('dap.ui.widgets');widgets.centered_float(widgets.scopes)<CR>",
		desc = "Float",
	},
	{ "<space>duh", "<cmd>lua require('dap.ui.widgets').hover()<CR>", desc = "Hover" },
})
