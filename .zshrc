if [[ -d "${HOME}/.oh-my-zsh" ]]; then
    # Path to your oh-my-zsh installation.
    export ZSH="${HOME}/.oh-my-zsh"
    plugins=(git zsh-vim-mode)
    source $ZSH/oh-my-zsh.sh
fi

function get_pwd() {
    echo "${PWD/$HOME/~}"
}

function git_branch() {
    local BRANCH="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"
    local STATUS="$(git status -sb 2> /dev/null)"
    test -n "${STATUS}" && echo "${STATUS}" | head -n 1 | grep -qv '\.\.\.' && local LOCAL='local'
    test -n "${STATUS}" && echo "${STATUS}" | head -n 1 | grep -q '\[gone\]' && local LOCAL='gone'
    local DIRTY="$(echo ${STATUS} | tail -n +2 | wc -l | awk '{print $1}')"
    [[ "${DIRTY}" -eq 0 ]] && DIRTY=''
    local AHEAD="$(echo ${STATUS} | grep ahead | sed 's/^.*ahead \([[:digit:]]*\).*$/ ↑\1/')"
    local BEHIND="$(echo ${STATUS} | grep behind | sed 's/^.*behind \([[:digit:]]*\).*$/ ↑\1/')"
    echo "${BRANCH:+(${BRANCH})}${LOCAL:+ [${LOCAL}]}${DIRTY:+ X${DIRTY}}${AHEAD}${BEHIND}"
}

setopt INTERACTIVECOMMENTS
setopt PROMPT_PERCENT
setopt PROMPT_SUBST
setopt AUTO_CD

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
function git_find_merge() {
    if [ -z "${1}" ]; then
        echo "Please enter a commit hash to find the merge of"
    else
        git rev-list ${1}..master --ancestry-path | grep -f <(git rev-list ${1}..master --first-parent) | tail -n 1
    fi
}

export EDITOR=/usr/local/bin/vim
export VISUAL=/usr/local/bin/vim

if uname -a | grep -q Darwin; then
    alias copy_last_command='fc -ln -1 | awk '\''{$1=$1}1'\'' ORS='\''\'' | pbcopy'
    alias copy_git_branch='git rev-parse --abbrev-ref HEAD | awk '\''{$1=$1}1'\'' ORS='\''\'' | pbcopy'
    export JAVA_HOME=$(/usr/libexec/java_home)
else
    export JAVA_HOME=/usr/lib/jvm/default-java
fi
export PATH="${JAVA_HOME}:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH:$HOME/.bin"
export PATH="/usr/local/sbin:$PATH"

bindkey "^R" history-incremental-search-backward
export TERM=xterm-256color
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

autoload -z edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

export PATH="$HOME/.cargo/bin:$PATH"

setopt COMPLETE_ALIASES
setopt AUTO_CD

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt APPENDHISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
