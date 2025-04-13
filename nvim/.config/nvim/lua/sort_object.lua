local ts_utils = require("nvim-treesitter.ts_utils")
local treesitter = require("vim.treesitter")

local M = {}

local function replace_node(sRow, sCol, eRow, eCol, text)
	local replaced_text = vim.fn.split(text, "\n")
	vim.api.nvim_buf_set_text(0, sRow, sCol, eRow, eCol, replaced_text)
end


local function sort(current_node)
	local child_count = current_node:named_child_count()
	local function is_sorted(idx, key_name)
		local child = current_node:named_child(idx)
		if child == nil then
			error("Child is nil, returning false")
			return false
		end
		local text
		if child:type() == "comment" then
			-- expand down until we see something that is not a comment
			for x = idx + 1, child_count - 1 do
				local next_node = current_node:named_child(x)
				local prev_node_type = next_node:type()
				if prev_node_type ~= "comment" then
					-- increment one more time since we are not seeing a comment
					text = vim.treesitter.get_node_text(next_node, 0)
					break
				end
			end
		elseif child:type() ~= "pair" then
			text = vim.treesitter.get_node_text(child, 0)
		else
			local child_child = child:named_child(0)
			if child_child == nil then
				return false
			end
			text = vim.treesitter.get_node_text(child_child, 0)
		end
		if not text then
			error("unable to find text")
		end
		return text < key_name
	end

	-- Insertion sort
	for i = 1, child_count - 1 do
		local key = current_node:named_child(i)
		if key == nil then
			goto continue
		end
		if key:type() == "comment" then
			goto continue
		end
		local key_child = key:named_child(0)
		local key_name
		if key_child == nil then
			key_name = vim.treesitter.get_node_text(key, 0)
		else
			key_name = vim.treesitter.get_node_text(key_child, 0)
		end
		local key_text = vim.treesitter.get_node_text(key, 0)
		if not key_name then
			error("unable to find pair key for node")
		end
		if not key_text then
			error("unable to find pair text for node")
		end

		local keySRow, keySCol, keyERow, keyECol = key:range()

		local skipped_count = 0
		for x = i - 1, 0, -1 do
			local prev_node = current_node:named_child(x)
			local prev_node_type = prev_node:type()
			if prev_node_type == "comment" then
				-- extend range
				local sRow, sCol, _, _ = prev_node:range()
				keySRow = sRow
				keySCol = sCol
				skipped_count = skipped_count + 1
			else
				break
			end
		end
		key_text = table.concat(vim.api.nvim_buf_get_text(0, keySRow, keySCol, keyERow, keyECol, {}), "\n")

		local comments_seen = 0

		local j = i - 1 - skipped_count

		while j >= 0 and not is_sorted(j, key_name) do
			local j_child = current_node:named_child(j)
			local j_child_plus_1 = current_node:named_child(j + 1)

			-- text_to_replace is the text we want to swap, the rows and cols
			-- are where it should go.
			local text_to_replace, curSRow, curSCol, curERow, curECol, nextSRow, nextSCol, nextERow, nextECol

			-- if not object, skip swap
			if j_child:type() == "comment" then
				local text = vim.treesitter.get_node_text(current_node:named_child(j), 0)
				curSRow, curSCol, curERow, curECol = current_node:named_child(j):range()

				-- expand up while we see comments
				for x = j - 1, 0, -1 do
					local prev_node = current_node:named_child(x)
					local prev_node_type = prev_node:type()
					if prev_node_type == "comment" then
						-- extend range
						local sRow, sCol, _, _ = prev_node:range()
						curSRow = sRow
						curSCol = sCol
						comments_seen = comments_seen + 1
					else
						break
					end
				end

				-- expand down until we see something that is not a comment
				for x = j + 1, child_count - 1 do
					local next_node = current_node:named_child(x)
					local prev_node_type = next_node:type()
					if prev_node_type == "comment" then
						-- extend range
						local _, _, eRow, eCol = next_node:range()
						curERow = eRow
						curECol = eCol
					else
						-- increment one more time since we are not seeing a comment
						local _, _, eRow, eCol = next_node:range()
						curERow = eRow
						curECol = eCol
						break
					end
				end

				text = table.concat(vim.api.nvim_buf_get_text(0, curSRow, curSCol, curERow, curECol, {}), "\n")
				text_to_replace = text
			else
				local text = vim.treesitter.get_node_text(current_node:named_child(j), 0)
				curSRow, curSCol, curERow, curECol = current_node:named_child(j):range()

				-- expand j
				for x = j - 1, 0, -1 do
					local prev_node = current_node:named_child(x)
					local prev_node_type = prev_node:type()
					if prev_node_type == "comment" then
						-- extend range
						local sRow, sCol, _, _ = prev_node:range()
						curSRow = sRow
						curSCol = sCol
					else
						break
					end
				end
				text = table.concat(vim.api.nvim_buf_get_text(0, curSRow, curSCol, curERow, curECol, {}), "\n")
				text_to_replace = text
			end

			if j_child_plus_1:type() == "comment" then
				nextSRow, nextSCol, nextERow, nextECol = j_child_plus_1:range()

				-- expand up while we see comments
				for x = j, 0, -1 do
					local prev_node = current_node:named_child(x)
					local prev_node_type = prev_node:type()
					if prev_node_type == "comment" then
						-- extend range
						local sRow, sCol, _, _ = prev_node:range()
						nextSRow = sRow
						nextSCol = sCol
						comments_seen = comments_seen + 1
					else
						break
					end
				end

				-- expand down until we see something that is not a comment
				for x = j + 2, child_count - 1 do
					local next_node = current_node:named_child(x)
					local prev_node_type = next_node:type()
					if prev_node_type == "comment" then
						-- extend range
						local _, _, eRow, eCol = next_node:range()
						nextERow = eRow
						nextECol = eCol
					else
						-- increment one more time since we are not seeing a comment
						local _, _, eRow, eCol = next_node:range()
						nextERow = eRow
						nextECol = eCol
						break
					end
				end
			else
				-- expand up while we see comments
				nextSRow, nextSCol, nextERow, nextECol = j_child_plus_1:range()
				for x = j, 0, -1 do
					local prev_node = current_node:named_child(x)
					if not prev_node then
						break
					end
					local prev_node_type = prev_node:type()
					if prev_node_type == "comment" then
						-- extend range
						local sRow, sCol, _, _ = prev_node:range()
						nextSRow = sRow
						nextSCol = sCol
					else
						break
					end
				end
			end

			replace_node(nextSRow, nextSCol, nextERow, nextECol, text_to_replace)
			treesitter.get_parser(0, "typescript"):parse()
			current_node = ts_utils.get_node_at_cursor()
			j = j - 1
		end
		local last_idx = j + 1
		local lastSRow, lastSCol, lastERow, lastECol = current_node:named_child(last_idx):range()

		if current_node:named_child(last_idx):type() == "comment" then
			-- expand up while we see comments
			for x = last_idx - 1, 0, -1 do
				local prev_node = current_node:named_child(x)
				local prev_node_type = prev_node:type()
				if prev_node_type == "comment" then
					-- extend range
					local sRow, sCol, _, _ = prev_node:range()
					lastSRow = sRow
					lastSCol = sCol
				else
					break
				end
			end

			-- expand down until we see something that is not a comment
			for x = last_idx + 1, child_count - 1 do
				local next_node = current_node:named_child(x)
				local prev_node_type = next_node:type()
				if prev_node_type == "comment" then
					-- extend range
					local _, _, eRow, eCol = next_node:range()
					lastERow = eRow
					lastECol = eCol
				else
					-- increment one more time since we are not seeing a comment
					local _, _, eRow, eCol = next_node:range()
					lastERow = eRow
					lastECol = eCol
					break
				end
			end
		else
			for x = last_idx - 1, 0, -1 do
				local prev_node = current_node:named_child(x)
				local prev_node_type = prev_node:type()
				if prev_node_type == "comment" then
					-- extend range
					local sRow, sCol, _, _ = prev_node:range()
					lastSRow = sRow
					lastSCol = sCol
				else
					break
				end
			end
		end
		replace_node(lastSRow, lastSCol, lastERow, lastECol, key_text)
		treesitter.get_parser(0, "typescript"):parse()
		current_node = ts_utils.get_node_at_cursor()
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
