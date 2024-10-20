# Set ZDOTDIR="$HOME/.config/zsh"
[ -z $THREADS ] && source "$HOME/.config/environment.d/env.conf"

# Oh My Zsh
export ZSH="$XDG_DATA_HOME/oh-my-zsh"
DISABLE_AUTO_UPDATE=true
ENABLE_CORRECTION=true
COMPLETION_WAITING_DOTS=true
plugins=(last-working-dir vi-mode zsh-autosuggestions zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(brackets line main pattern regexp root)
ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST
source "$ZSH/oh-my-zsh.sh"

# Disable syntax highlighting in DAV directories
nohl_dav() {
	[[ $PWD =~ .*gvfs/dav.* && -z $nohl ]] && \
		nohl=true && shift -p plugins && source $ZSH/oh-my-zsh.sh || \
		{[ $nohl ] && unset nohl && plugins+=zsh-syntax-highlighting && source $ZSH/oh-my-zsh.sh}
}
add-zsh-hook chpwd nohl_dav

# Fixing pasting
pasteinit() {
	OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
	zle -N self-insert url-quote-magic
}
pastefinish() {
	zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish

PS1="%F{cyan}%(?..%? )%F{cyan}%B░▒▓█%F{bright-wh}%K{cyan}%n@%m%F{cyan}%K{txtrst}█▓▒░ %F{cyan}%B%~ %% "

# Short aliases
alias h="htop"
alias l="ls -l --human-readable --all"
alias n="ncmpcpp"
alias r="ranger"
alias v="vim"
alias y="yay"

# Swapping aliases
alias normalcaps="swaymsg input '*' xkb_options escape:escape"
alias swapcaps="swaymsg input '*' xkb_options caps:swapescape"

# Safety aliases
alias cp="cp --interactive"
alias mv="mv --interactive"

# Convenience aliases
alias config='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias run_gha='grep run: .github/**/*yml | cut -d: -f2 | sh -e'
alias pkgcl='pacman --remove --cascade --recursive $(pacman --query --deps --quiet --unrequired --unrequired)'

neofetch
