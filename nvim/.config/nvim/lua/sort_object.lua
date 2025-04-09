local ts_utils = require("nvim-treesitter.ts_utils")
local treesitter = require("vim.treesitter")

local M = {}

local function replace_node(node, text)
	if not node or not text then
		error("cannot replace text")
		return
	end

	if type(text) ~= "string" then
		text = table.concat(text, "\n")
	end

	local sRow, sCol, eRow, eCol = node:range()

	local replaced_text = vim.fn.split(text, "\n")
	vim.api.nvim_buf_set_text(0, sRow, sCol, eRow, eCol, replaced_text)
end

local function sort(current_node)
	local function is_sorted(idx, key_name)
		local j_child = current_node:named_child(idx)
		if j_child == nil then
			return false
		end
		local j_child_child = j_child:named_child(0)
		if j_child_child == nil then
			return false
		end
		return vim.treesitter.get_node_text(j_child_child, 0) < key_name
	end

	local child_count = current_node:named_child_count()

	-- Insertion sort
	for i = 1, child_count - 1 do
		local num_non_swaps = 0
		local key = current_node:named_child(i)
		if key == nil then
			goto continue
		end
		if key:type() ~= "pair" then
			goto continue
		end
		local key_child = key:named_child(0)
		if key_child == nil then
			goto continue
		end
		local key_name = vim.treesitter.get_node_text(key_child, 0)
		local key_text = vim.treesitter.get_node_text(key, 0)
		if not key_name then
			error("unable to find pair key for node")
		end
		if not key_text then
			error("unable to find pair text for node")
		end
		local j = i - 1

		while j >= 0 and not is_sorted(j, key_name) do
			-- if not object, skip swap
			if current_node:named_child(j):type() == "pair" and current_node:named_child(j + 1 + num_non_swaps):type() == "pair" then
				local text = vim.treesitter.get_node_text(current_node:named_child(j), 0)
				replace_node(current_node:named_child(j + 1 + num_non_swaps), text)
				treesitter.get_parser(0, "typescript"):parse()
				current_node = ts_utils.get_node_at_cursor()
				num_non_swaps = 0
			else
				num_non_swaps = num_non_swaps + 1
			end
			j = j - 1
		end
		if current_node:named_child(j + 1 + num_non_swaps):type() == "pair" then
			replace_node(current_node:named_child(j + 1 + num_non_swaps), key_text)
			treesitter.get_parser(0, "typescript"):parse()
			current_node = ts_utils.get_node_at_cursor()
		end
		::continue::
	end

	-- sort nested objects
	current_node = ts_utils.get_node_at_cursor()
	local orig = vim.api.nvim_win_get_cursor(0)
	for i = 0, child_count - 1 do
		local new_node = current_node:named_child(i)
		if new_node then
			if new_node:named_child_count() > 1 then
				if new_node:named_child_count() >= 2 and new_node:named_child(1):type() == "object" then
					local child_object = new_node:named_child(1)
					ts_utils.goto_node(child_object, false, true)
					sort(child_object)
					vim.api.nvim_win_set_cursor(0, orig)
					treesitter.get_parser(0, "typescript"):parse()
					current_node = ts_utils.get_node_at_cursor()
				end
			end
		end
	end
	vim.api.nvim_win_set_cursor(0, orig)
end

function M.sort_object()
	local lang = vim.bo.ft
	if lang ~= "javascript" and lang ~= "typescript" then
		return
	end

	local current_node = ts_utils.get_node_at_cursor()

	if not current_node then
		print("Unable to get current node")
		return
	end

	local current_type = current_node:type()
	local isPair = current_type == "pair"
	local isObject = current_type == "object"

	if not isPair and not isObject then
		return
	end

	if isPair then
		while not isObject do
			current_node = ts_utils.get_node_at_cursor()
			isObject = current_type == "object"
		end
	end

	local orig = vim.api.nvim_win_get_cursor(0)

	vim.api.nvim_win_set_cursor(0, orig)

	treesitter.get_parser(0, "typescript"):parse()
	current_node = ts_utils.get_node_at_cursor()

	sort(current_node)

	vim.api.nvim_win_set_cursor(0, orig)
end

function M.goto_top_object()
	vim.cmd("normal $")
	local lang = vim.bo.ft
	if lang ~= "javascript" and lang ~= "typescript" then
		return
	end

	local current_node = ts_utils.get_node_at_cursor()

	if not current_node then
		print("Unable to get current node")
		return
	end

	while current_node and (current_node:parent():type() == "object" or current_node:parent():type() == "pair") do
		current_node = current_node:parent()
	end

	local sRow, sCol, _, _ = current_node:range()
	vim.api.nvim_win_set_cursor(0, { sRow + 1, sCol })
end

function M.goto_top_object_and_sort()
	M.goto_top_object()
	M.sort_object()
end

vim.api.nvim_exec([[command! SortObject :lua require('sort_object').goto_top_object_and_sort()]], false)

return M
