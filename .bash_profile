set -o vi

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

# default : \h:\W \u\$ 

function my_prompt {
    #local __time="\[\033[38;5;22m\]\T"

	local __cur_exit="$?"
	local __success_color="\033[38;5;2m\]" #green 28, 64
	local __failure_color="\033[38;5;160m\]" #red 124, 160
	if [ "$__cur_exit" == "0" ]
	then
		local __user_color=${__success_color}
	else
		local __user_color=${__failure_color}
	fi

    #local __user_color="\033[38;5;202m\]" #28 5 127 pink orange 172
    local __at_color="\033[38;5;166m\]" #202 orange, 166
    local __host_color="\033[38;5;37m\]" # teal 30, 37
    local __user_and_host="[$__user_color\u$__at_color@$__host_color\h\e[39m]"

    local __cur_loc_color="\[\033[39m\]" #6
    local __cur_location="$__cur_loc_color\w"

    local __date_time="\D{%c}"

    local __git_branch_color="\[\033[38;5;2m\]" #33 2
    local __git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'
    #local __git_branch="($(git rev-parse --abbrev-ref HEAD))"
    local __prompt_tail="\n\[\033[00;00m\]\$"
    local __last_color="\[\033[00m\]"

    export PS1="$__user_and_host $__cur_location $__date_time $__git_branch_color$__git_branch$__prompt_tail$__last_color "
}
PROMPT_COMMAND="my_prompt; $PROMPT_COMMAND"
export EDITOR=/usr/local/bin/vim
export VISUAL=/usr/local/bin/vim

export JAVA_HOME=$(/usr/libexec/java_home))
export PATH="${JAVA_HOME}:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH:$HOME/bin"
