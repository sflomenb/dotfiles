#!/usr/bin/env bash

usage() {
    echo "git-cleanup-remote.sh - deletes remote branches"
    echo ""
    echo "git-cleanup-remote.sh [-f] [-d date] [-i pattern]"
    echo "                      [-e pattern] [-a pattern]"
    echo "                      [-b num] [-n] [-c]"
    echo "       where  -f         : actually perform the deletion on the remote"
    echo "       where  -d date    : date the latest commit should be after"
    echo "       where  -i pattern : pattern should in included in the branch name"
    echo "       where  -e pattern : pattern should in excluded in the branch name"
    echo "       where  -a pattern : pattern should be included in the author's name"
    echo "       where  -b num     : number of commits behind master"
    echo "       where  -n         : look at all branches, not just merged branches"
    echo "       where  -c         : copy first branch found to clipboard"
    echo ""
    exit 1
}

while getopts "fd:i:e:a:b:nch" opt; do
    case $opt in
        f)
            FORCE="y"
            ;;
        d)
            DATE=$OPTARG
            ;;
        i)
            INCLUDES=$OPTARG
            ;;
        e)
            EXCLUDES=$OPTARG
            ;;
        a)
            AUTHOR=$OPTARG
            ;;
        b)
            AMOUNT_BEHIND=$OPTARG
            ;;
        n)
            MERGED="y"
            ;;
        c)
            COPY="y"
            ;;
        h)
            usage
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            usage
            ;;
    esac
done

COUNTER=0
BRANCHES=""
for k in $(git branch -r --sort=-committerdate $(if [[ -z "${MERGED}" ]]; then echo '--merged origin/master'; fi) | sed /master/d); do
    if [[ (-z "${DATE}" || -z "$(git log -1 --since=''"${DATE}"'' -s $k)") && ( -z "${INCLUDES}" || "${k}" =~ ${INCLUDES} ) && ( -z "${EXCLUDES}" || ( ! "${k}" =~ ${EXCLUDES} ) ) && ( -z "${AUTHOR}" || $(git log $k -1 --pretty=%an) =~ ${AUTHOR} ) && ( -z "${AMOUNT_BEHIND}" || $(git rev-list --count --left-right origin/master..."${k}" | awk '{print $1}') -gt "${AMOUNT_BEHIND}" ) ]]; then
        (( COUNTER++ ))
        echo "${k#*/}"
        if [[ -z "${BRANCHES}" && -n "${COPY}" ]]; then
            echo -n "${k#*/}" | pbcopy
        fi
        BRANCHES="${BRANCHES}${k#*/} "
    fi
done

echo "${COUNTER} branches found"
TOTAL="$(git branch -r | wc -l | awk '{print $1}')"
echo "$(bc <<< "scale=2; ${COUNTER}/${TOTAL}*100")% of ${TOTAL} branches"

if [[ -n "${BRANCHES}" ]]; then
    if [[ -z "${FORCE}" ]]; then
        echo "git push origin --delete ${BRANCHES}"
    else
        git push origin --delete ${BRANCHES}
    fi
else
    echo "No branches to delete"
fi
