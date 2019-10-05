export PATH="/home/jakob/.local/bin:$PATH"

export ZSH="/home/jakob/.oh-my-zsh"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(command-not-found last-working-dir thefuck vi-mode zsh-autosuggestions zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(brackets line main pattern regexp root)
SAVEHIST=1000
source $ZSH/oh-my-zsh.sh

autoload -U compinit
compinit

export PS1="%F{green}%B%K{green}█▓▒░%F{white}%K{green}%n@%m%F{green}%K{txtrst}█▓▒░ %F{green}%B%~ %% "

alias l="ls -lha"
alias v="vim"
alias r="ranger"
alias lock="xlock -mode blank"
alias susp="sudo pm-suspend && lock"
alias config='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

alias update="sudo emerge-webrsync"
alias upgrade="echo 'Remember to upgrade your /usr/local regularly' && sudo emerge -avuDN --keep-going --with-bdeps=y @world"
alias pkgcl="sudo emerge -a --depclean"
alias srccl="sudo eclean-dist -d"

sed -i "/^: [0-9]*:0;mpv/d" ~/.zsh_history
neofetch
