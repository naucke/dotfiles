export PATH="$PATH:$HOME/.local/bin"

export ZSH="/home/jakob/.oh-my-zsh"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
plugins=(last-working-dir vi-mode zsh-autosuggestions zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(brackets line main pattern regexp root)
SAVEHIST=1000
source $ZSH/oh-my-zsh.sh

pasteinit() {
  OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
  zle -N self-insert url-quote-magic
}
pastefinish() {
  zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish

export EDITOR=vim
export THREADS=8
export MAKEFLAGS="-j$THREADS"
PS1="%F{cyan}%(?..%? )%F{cyan}%B░▒▓█%F{white}%K{cyan}%n@%m%F{cyan}%K{txtrst}█▓▒░ %F{cyan}%B%~ %% "

alias j="joplin"
alias l="ls -l --human-readable --all"
alias m="mutt"
alias n="ncmpcpp"
alias r="ranger"
alias v="vim"

alias cp="cp --interactive"
alias mv="mv --interactive"

alias lock_screenon="i3lock --image ~/Desktop/Desktop.png"
alias lock="lock_screenon && xset dpms force off"

alias nosleep="xset -dpms && xset s off"
alias dosleep="xset s on"
alias normalcaps="setxkbmap -option && setxkbmap de"
alias swapcaps="setxkbmap -option caps:swapescape"

alias config='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias run_gh_actions='grep run: .github/**/*yml | cut -d: -f2 | sh -e'
alias pkgcl='pacman --remove --cascade --recursive $(pacman --query --deps --quiet --unrequired --unrequired)'

alias boincsusp="sh -c 'cd /var/lib/boinc && boinccmd --set_run_mode never'"
alias boincres="sh -c 'cd /var/lib/boinc && boinccmd --set_run_mode auto'"

neofetch
