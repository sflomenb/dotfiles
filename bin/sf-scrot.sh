#!/usr/bin/env bash

scrot -F "${HOME}/Downloads/screenshots/%F-%T.png" -s -f -e 'xclip -selection clipboard -t image/png -i $f'
