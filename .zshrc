if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

prompt adam1
alias l='ls -la'
alias v='vim'
alias r='ranger'
alias win='sudo /home/jakob/Documents/windows.sh'
alias susp='systemctl suspend'
alias update='yay -Syu'
alias autorm='sudo pacman -Rcs $(pacman -Qdtq)'
alias config='/usr/bin/git --git-dir=/home/jakob/Documents/dotfiles --work-tree=$HOME'
neofetch
