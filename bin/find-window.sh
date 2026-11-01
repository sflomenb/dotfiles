#!/usr/bin/env bash

SELECTED=$(tmux lsw -a -F '#{session_name}:#{window_index} | #{window_name}' | fzf-tmux | cut -d'|' -f 1 | sed 's/\s*$//')
[[ -n "${SELECTED}" ]] && tmux switch-client -t "${SELECTED}"
