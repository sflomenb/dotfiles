local dap = require("dap")

dap.adapters.lldb = {
	type = "executable",
	command = "",
	name = "lldb",
}

require("dapui").setup()

require("which-key").register({
	d = {
		name = "Debug",
		s = {
			name = "Step",
			c = { "<cmd>lua require('dap').continue()<CR>", "Continue" },
			v = { "<cmd>lua require('dap').step_over()<CR>", "Step Over" },
			i = { "<cmd>lua require('dap').step_into()<CR>", "Step Into" },
			o = { "<cmd>lua require('dap').step_out()<CR>", "Step Out" },
			u = { "<cmd>lua require('dap').run_to_cursor()<CR>", "Run To Cursor" },
		},
		h = {
			name = "Hover",
			-- h = { "<cmd>lua require('dap.ui.variables').hover()<CR>", "Hover" },
			h = { "<cmd>lua require('dap.ui.widgets').hover()<CR>", "Hover" },
			v = { "<cmd>lua require('dap.ui.widgets').visual_hover()<CR>", "Visual Hover" },
		},
		u = {
			name = "UI",
			h = { "<cmd>lua require('dap.ui.widgets').hover()<CR>", "Hover" },
			f = { "local widgets=require('dap.ui.widgets');widgets.centered_float(widgets.scopes)<CR>", "Float" },
		},
		r = {
			name = "Repl",
			o = { "<cmd>lua require('dap').repl.open()<CR>", "Open" },
			l = { "<cmd>lua require('dap').repl.run_last()<CR>", "Run Last" },
		},
		b = {
			name = "Breakpoints",
			c = {
				"<cmd>lua require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>",
				"Breakpoint Condition",
			},
			l = {
				"<cmd>lua require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>",
				"Log Point Message",
			},
			t = { "<cmd>lua require('dap').toggle_breakpoint()<CR>", "Toggle" },
			d = { "<cmd>lua require('dap').clear_breakpoints()<CR>", "Delete" },
		},
		c = { "<cmd>lua require('dap.ui.variables').scopes()<CR>", "Scopes" },
		i = { "<cmd>lua require('dapui').toggle()<CR>", "Toggle" },
		I = { "<cmd>lua require('dapui').toggle({reset=true})<CR>", "Toggle and reset layout" },
		t = { "<cmd>lua require('dap').terminate()<CR>", "Terminate" },
	},
}, { prefix = "<space>" })
