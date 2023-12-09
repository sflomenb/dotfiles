#!/usr/bin/env bash

function change_neovim_color {
    local DIR=''
    if uname -a | grep -qi 'darwin'; then
        local DIR="${TMPDIR}"
    else
        local DIR="${XDG_RUNTIME_DIR}"
    fi
    if [[ -d "${DIR}" ]]; then
        # https://www.reddit.com/r/neovim/comments/y4z2ew/does_neovim_automatically_create_server_pipes_in/
        find $DIR/*nvim* \
            -maxdepth 3 \
            -type s \
            -name 'nvim*.*0' \
            -exec nvim --remote-send '<C-\><C-N>:lua require("color").update_color()<CR>' --server {} \;
    fi
}

function change_kitty_color {
    local hour=$(date '+%-H')
    if [[ "${hour}" -gt 6 && "${hour}" -lt 18 ]]; then
        local color='Latte'
    else
        local color='Macchiato'
    fi
    kitty +kitten themes --reload-in=all "Catppuccin-${color}"
}

change_neovim_color
change_kitty_color
