#!/usr/bin/env bash

SELECTED=$(tmux ls | fzf-tmux | cut -d: -f 1)
[[ -n "${SELECTED}" ]] && tmux switch-client -t "${SELECTED}"
