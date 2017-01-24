#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
eval "`dircolors -b ~/.dircolors`"
alias ls='ls --color=auto'


PS1='\[\033[01;38;5;202m\][\u@\h \W]\$ '
