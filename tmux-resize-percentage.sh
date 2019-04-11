#!/usr/bin/env bash
while getopts p:t: opt;
do
    case $opt in
    p)
        PERCENTAGE="$OPTARG"
		;;
    t)
        TYPE="$OPTARG"
		;;
    ?)
		printf "Usage: %s: [-p percentage] [-t type] \n" $0
		exit 2
		;;
    esac
done
if [[ -z "${PERCENTAGE}" ]]; then
    echo "Please enter a percentage using -p"
    exit 1
elif [[ -z "${TYPE}" ]]; then
    echo "Please enter a type using -t"
    exit 1
fi
if [[ "${TYPE}" == "height" ]]; then
    FLAG="-y"
    NEW_SIZE=$(expr $(tmux display -p '#{window_height}') \* ${PERCENTAGE} \/ 100)
elif [[ "${TYPE}" == "width" ]]; then
    FLAG="-x"
    NEW_SIZE=$(expr $(tmux display -p '#{window_width}') \* ${PERCENTAGE} \/ 100)
fi
tmux resize-pane ${FLAG} ${NEW_SIZE}
