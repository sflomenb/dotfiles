#!/usr/bin/env bash

# Inspired by ThePrimeagen's workflow
# https://www.youtube.com/watch?v=bdumjiHabhQ

if [[ -z "${PROJECT_DIR}" ]]; then
    echo "Please ensure a dir is set via PROJECT_DIR" >&2
    exit 1
fi

# echo to split stringinto different arguments.
NEW_DIR=$(find $(echo "${PROJECT_DIR}") -maxdepth 3 -type d | fzf-tmux)

if [[ -z "${NEW_DIR}" ]]; then
    echo "Please select a dir" >&2
    exit 1
fi

SESSION_NAME=$(basename "${NEW_DIR}")

# Create session if it doesn't exist.
if ! tmux list-sessions -F '#{session_name}' | grep -q "${SESSION_NAME}"; then
    tmux new-session -d -s "${SESSION_NAME}" -c "${NEW_DIR}"
fi

# Create magit window if it doesn't exist.
if ! tmux list-windows -t "${SESSION_NAME}" -F '#{window_name}' | grep -q magit; then
    tmux new-window -ad -n magit -t "${SESSION_NAME}:1" -c "${NEW_DIR}" "$(which zsh) -ic 'emacs -f magit-status -f delete-other-windows'"
fi

if [[ -n "${TMUX}" ]]; then
    # Switch to session if we are already in tmux.
    tmux switch-client -t "${SESSION_NAME}"
else
    # Attach to session if we are not in tmux.
    tmux a -t "${SESSION_NAME}"
fi

