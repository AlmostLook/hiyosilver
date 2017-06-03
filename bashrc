#LS DIRCOLORS
[[ $- != *i* ]] && return
eval "`dircolors -b ~/.dircolors`"
alias ls='ls --color=auto --file-type'

#PROMPT
PS1='\[\033[01;38;5;202m\] \w $\[\e[0m\] '
