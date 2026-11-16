#!/usr/bin/env bash

SELECTED=$(tmux lsw -a -F '#{session_name}:#{window_index}|#{window_name}' | fzf-tmux | cut -d'|' -f 1)
[[ -n "${SELECTED}" ]] && tmux switch-client -t "${SELECTED}"
