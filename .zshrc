# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

plugins=(git zsh-vim-mode)

source $ZSH/oh-my-zsh.sh

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
${MODE_INDICATOR_PROMPT} %(?.$fg[green].$fg[red])%n$fg[magenta]@$fg[cyan]%m%{$reset_color%}: $(get_pwd) %W %t $fg[green]$(git_branch) %{$reset_color%}
%# '

bindkey -v

alias ls="ls -GF"
alias brewup='brew update; brew upgrade; brew cleanup; brew doctor'
alias ll="ls -l"
alias la="ls -A"
alias grep="grep --color=auto"
alias copy_last_command='fc -ln -1 | awk '\''{$1=$1}1'\'' ORS='\''\'' | pbcopy'
alias copy_git_branch='git rev-parse --abbrev-ref HEAD | awk '\''{$1=$1}1'\'' ORS='\''\'' | pbcopy'
function git_find_merge() {
    if [ -z "${1}" ]; then
        echo "Please enter a commit hash to find the merge of"
    else
        git rev-list ${1}..master --ancestry-path | grep -f <(git rev-list ${1}..master --first-parent) | tail -n 1
    fi
}

export EDITOR=/usr/local/bin/vim
export VISUAL=/usr/local/bin/vim

if uname -a | grep Darwin; then
    export JAVA_HOME=$(/usr/libexec/java_home)
else
    export JAVA_HOME=/usr/lib/jvm/default-java
fi
export PATH="${JAVA_HOME}:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH:$HOME/bin"
