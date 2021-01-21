if [[ -d "${HOME}/.oh-my-zsh" ]]; then
    # Path to your oh-my-zsh installation.
    export ZSH="${HOME}/.oh-my-zsh"
    plugins=(git zsh-vim-mode docker docker-compose zsh-syntax-highlighting zsh-autosuggestions)
    source $ZSH/oh-my-zsh.sh
fi

function git_branch() {
    local BRANCH="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"
    local STATUS="$(git status -sb 2> /dev/null)"
    test -n "${STATUS}" && echo "${STATUS}" | head -n 1 | grep -qv '\.\.\.' && local LOCAL='local'
    test -n "${STATUS}" && echo "${STATUS}" | head -n 1 | grep -q '\[gone\]' && local LOCAL='gone'
    local DIRTY="$(echo ${STATUS} | tail -n +2 | wc -l | awk '{print $1}')"
    [[ "${DIRTY}" -eq 0 ]] && DIRTY=''
    local AHEAD="$(echo ${STATUS} | grep ahead | sed 's/^.*ahead \([[:digit:]]*\).*$/ ↑\1/')"
    local BEHIND="$(echo ${STATUS} | grep behind | sed 's/^.*behind \([[:digit:]]*\).*$/ ↓\1/')"
    echo "${BRANCH:+(${BRANCH})}${LOCAL:+ [${LOCAL}]}${DIRTY:+ X${DIRTY}}${AHEAD}${BEHIND}"
}

setopt INTERACTIVECOMMENTS
setopt PROMPT_PERCENT
setopt PROMPT_SUBST
setopt AUTO_CD

autoload -U colors && colors

export PROMPT_PERCENT_OF_LINE=20

function myPromptWidth() {
  echo $(( ${COLUMNS:-80} * PROMPT_PERCENT_OF_LINE / 100 ))
}

PROMPT='${MODE_INDICATOR_PROMPT}%(?..$fg[red]%? )$fg[green]%n$fg[magenta]@$fg[cyan]%m%{$reset_color%}: %$(myPromptWidth)<..<%~%<< %W %t $fg[green]$(git_branch) %{$reset_color%}
%# '

bindkey -v

ls -GF > /dev/null 2>&1 && alias ls="ls -GF"
[[ -x "$(command -v brew)" ]] && alias brewup='brew update; brew upgrade; brew cleanup; brew doctor'
alias ll="ls -l"
alias la="ls -A"
grep 2>&1 | grep -q color && alias grep="grep --color=auto"
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
    alias copy_last_command='fc -ln -1 | tr -d '"'"'\n'"'"' | pbcopy'
    alias copy_git_branch='git rev-parse --abbrev-ref HEAD | awk '\''{$1=$1}1'\'' ORS='\''\'' | pbcopy'
    export JAVA_HOME=$(/usr/libexec/java_home)
elif [[ -e /etc/os-release ]] && grep -qi alpine /etc/os-release; then
    alias apkup='apk update && apk upgrade'
else
    export JAVA_HOME=/usr/lib/jvm/default-java
fi
export PATH="${JAVA_HOME}:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH:$HOME/.bin"
export PATH="/usr/local/sbin:$PATH"

bindkey "^R" history-incremental-search-backward
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

autoload -z edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

export PATH="$HOME/.cargo/bin:$PATH"
[[ -d /usr/local/go/bin ]] && export "PATH=$PATH:/usr/local/go/bin"

setopt COMPLETE_ALIASES
setopt AUTO_CD

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt APPENDHISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt EXTENDED_GLOB
setopt GLOBSTARSHORT

[[ -d "/usr/local/share/git-core/contrib/diff-highlight" ]] && export PATH="${PATH}:/usr/local/share/git-core/contrib/diff-highlight"
[[ -x "/usr/local/share/diff-highlight" ]] && export PATH="${PATH}:/usr/local/share/diff-highlight"

function edit() {
    local PATTERN="${1:?Please enter a pattern}"
    local MY_GIT_DIR=$(git rev-parse --show-toplevel)
    shift
    local OPTS="$@"
    ${EDITOR} ${OPTS} $(git status --porcelain | grep "${PATTERN}" | awk '{print $NF}' | sed 's#^#'"${MY_GIT_DIR}"'/#g')
}

[[ "${TERM_PROGRAM:l}" == 'apple_terminal' ]] && test -x "$HOME/.vim/plugged/gruvbox/gruvbox_256palette.sh" && source $_

export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
    vim -R -c 'set ft=man nomod nolist nonumber norelativenumber' -c 'map q :q<CR>' \
    -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
    -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""

return 0

