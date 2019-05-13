if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

alias l="ls -la"
alias susp="sudo pm-suspend && xlock -mode blank"
alias config='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

alias update="sudo emerge-webrsync"
alias upgrade="echo 'Remember to upgrade your /usr/local regularly' && sudo emerge -avuDN --keep-going --with-bdeps=y @world"
alias pkgcl="sudo emerge -a --depclean"
alias srccl="sudo eclean-dist --deep"

neofetch
