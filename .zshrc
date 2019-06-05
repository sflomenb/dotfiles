function get_pwd() {
    echo "${PWD/$HOME/~}"
}

function git_branch() {
    local BRANCH="($(git rev-parse --abbrev-ref HEAD 2> /dev/null))"
    [[ "${BRANCH}" == '()' ]] && BRANCH=''
    local STATUS="$(git status --porcelain 2> /dev/null)"
    [[ -n "${STATUS}" ]] && STATUS=' x'
    echo "${BRANCH}${STATUS}"
}

setopt PROMPT_PERCENT
setopt PROMPT_SUBST

autoload -U colors && colors

PROMPT='
%(?.$fg[green].$fg[red])%n$fg[magenta]@$fg[cyan]%m%{$reset_color%}: $(get_pwd) %W %t $fg[green]$(git_branch) %{$reset_color%}
%# '
