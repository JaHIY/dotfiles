# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

#oh-my-zsh
export ZSH="$HOME/.zplug/repos/robbyrussell/oh-my-zsh"
find "$ZSH/lib" -type f -iregex '.*\.zsh' -print0 | sort -z | \
    while IFS='' read -r -d $'\0' config_file; do
        source "$config_file"
    done

#zplug
source "$HOME/.zplug/init.zsh"

zplug 'plugins/adb', from:oh-my-zsh, if:'(( $+commands[adb] ))'
zplug 'plugins/ant', from:oh-my-zsh, if:'(( $+commands[ant] ))'
zplug 'plugins/archlinux', from:oh-my-zsh, if:'grep -s -q -F 'Arch Linux' /etc/os-release'
zplug 'plugins/catimg', from:oh-my-zsh, if:'(( $+commands[convert] ))'
zplug 'plugins/colored-man-pages', from:oh-my-zsh
zplug 'plugins/command-not-found', from:oh-my-zsh
zplug 'plugins/common-aliases', from:oh-my-zsh
zplug 'plugins/copydir', from:oh-my-zsh
zplug 'plugins/copyfile', from:oh-my-zsh
zplug 'plugins/cp', from:oh-my-zsh
zplug 'plugins/cpanm', from:oh-my-zsh, if:'(( $+commands[cpanm] ))'
zplug 'plugins/emoji-clock', from:oh-my-zsh
zplug 'plugins/emoji', from:oh-my-zsh
zplug 'plugins/encode64', from:oh-my-zsh
zplug 'plugins/extract', from:oh-my-zsh
zplug 'plugins/git', from:oh-my-zsh, if:'(( $+commands[git] ))', defer:2
zplug 'plugins/gnu-utils', from:oh-my-zsh
zplug 'plugins/golang', from:oh-my-zsh, if:'(( $+commands[go] ))', defer:2
zplug 'plugins/gradle', from:oh-my-zsh, if:'(( $+commands[gradle] ))'
zplug 'plugins/httpie', from:oh-my-zsh, if:'(( $+commands[httpie] ))'
zplug 'plugins/jsontools', from:oh-my-zsh
zplug 'plugins/lein', from:oh-my-zsh, if:'(( $+commands[lein] ))', defer:2
zplug 'plugins/mercurial', from:oh-my-zsh, if:'(( $+commands[hg] ))'
zplug 'plugins/npm', from:oh-my-zsh, if:'(( $+commands[npm] ))'
zplug 'plugins/perl', from:oh-my-zsh, if:'(( $+commands[perl] ))'
zplug 'plugins/python', from:oh-my-zsh, if:'(( $+commands[python] ))'
zplug 'plugins/rebar', from:oh-my-zsh, if:'(( $+commands[rebar] ))'
zplug 'plugins/rsync', from:oh-my-zsh, if:'(( $+commands[rsync] ))'
zplug 'plugins/ruby', from:oh-my-zsh, if:'(( $+commands[ruby] ))'
zplug 'plugins/rust', from:oh-my-zsh, if:'(( $+commands[rust] ))'
zplug 'plugins/rvm', from:oh-my-zsh, if:'(( $+commands[rvm] ))'
zplug 'plugins/sbt', from:oh-my-zsh, if:'(( $+commands[sbt] ))'
zplug 'plugins/scala', from:oh-my-zsh, if:'(( $+commands[scala] ))'
zplug 'plugins/screen', from:oh-my-zsh, if:'(( $+commands[screen] ))'
zplug 'plugins/sudo', from:oh-my-zsh, if:'(( $+commands[sudo] ))'
zplug 'plugins/svn', from:oh-my-zsh, if:'(( $+commands[svn] ))'
zplug 'plugins/systemd', from:oh-my-zsh, if:'(( $+commands[systemctl] ))'
zplug 'plugins/themes', from:oh-my-zsh
zplug 'plugins/tmux', from:oh-my-zsh, if:'(( $+commands[tmux] ))', defer:2
zplug 'plugins/torrent', from:oh-my-zsh
zplug 'plugins/urltools', from:oh-my-zsh
zplug 'plugins/zsh_reload', from:oh-my-zsh

zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'zsh-users/zsh-history-substring-search', defer:2
zplug 'zsh-users/zsh-autosuggestions', defer:2
zplug 'zsh-users/zsh-completions', defer:2

zplug 'JaHIY/zsh-theme-verse', as:theme

zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf 'Install? [y/N]: '
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose

autoload -U compinit && compinit
