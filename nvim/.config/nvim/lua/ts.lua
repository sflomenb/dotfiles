local langs = { "rust", "typescript", "tsx", "go", "javascript", "python", "lua", "query", "sql", "java" }
if not vim.fn.executable('nix') then
	require'nvim-treesitter'.install(langs)
end

vim.api.nvim_create_autocmd('FileType', {
  pattern = langs,
  callback = function()
	  local max_filesize = 100 * 1024
	  local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(0))
	  if ok and stats and stats.size < max_filesize then
		  vim.treesitter.start()
	  end
  end,
})

require("treesitter-context").setup()

local M = {}

--- Sets the current position to the start of the node.
--- @param node TSNode
function M.goto_node(node)
    local r, c = node:start()
    vim.api.nvim_win_set_cursor(0, {r+1, c})
end

return M
