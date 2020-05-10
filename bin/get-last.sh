#!/usr/bin/env bash

CMD="${1}"
DIR="${2}"
[[ -z "${CMD}" || -z "${DIR}" ]] && echo "Please enter a command and filename" >&2 && exit 1
(cd ${DIR} && ${CMD} $(ls -1tr | tail -n 1))
