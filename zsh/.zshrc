# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

#zinit
source "$HOME/.zinit/bin/zinit.zsh"

setopt promptsubst

zinit for OMZ::lib/{bzr,clipboard,compfix,completion,correction,diagnostics,directories,functions,git,grep,history,key-bindings,misc,nvm,prompt_info_functions,spectrum,termsupport,theme-and-appearance}.zsh

zinit ice wait as'completion' if'(( $+commands[adb] ))' lucid
zinit snippet 'OMZ::plugins/adb/_adb'

zinit ice wait if"grep -s -q -F 'Arch Linux' /etc/os-release" lucid
zinit snippet 'OMZ::plugins/archlinux/archlinux.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/command-not-found/command-not-found.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/common-aliases/common-aliases.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/cp/cp.plugin.zsh'

zinit ice wait if'(( $+commands[cpanm] ))' lucid
zinit snippet 'OMZ::plugins/cpanm/cpanm.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/encode64/encode64.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/extract/extract.plugin.zsh'

zinit ice wait if'(( $+commands[git] ))' lucid
zinit snippet 'OMZ::plugins/git/git.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/gnu-utils/gnu-utils.plugin.zsh'

zinit ice wait if'(( $+commands[go] ))' lucid
zinit snippet 'OMZ::plugins/golang/golang.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'

zinit ice wait if'(( $+commands[hg] ))' lucid
zinit snippet 'OMZ::plugins/mercurial/mercurial.plugin.zsh'

zinit ice wait if'(( $+commands[npm] ))' lucid
zinit snippet 'OMZ::plugins/npm/npm.plugin.zsh'

zinit ice wait if'(( $+commands[perl] ))' lucid
zinit snippet 'OMZ::plugins/perl/perl.plugin.zsh'

zinit ice wait if'(( $+commands[python] ))' lucid
zinit snippet 'OMZ::plugins/python/python.plugin.zsh'

zinit ice wait as'completion' if'(( $+commands[rebar] ))' lucid
zinit snippet 'OMZ::plugins/rebar/_rebar'

zinit ice wait if'(( $+commands[rsync] ))' lucid
zinit snippet 'OMZ::plugins/rsync/rsync.plugin.zsh'

zinit ice wait if'(( $+commands[ruby] ))' lucid
zinit snippet 'OMZ::plugins/ruby/ruby.plugin.zsh'

zinit ice wait if'(( $+commands[screen] ))' lucid
zinit snippet 'OMZ::plugins/screen/screen.plugin.zsh'

zinit ice wait if'(( $+commands[sudo] ))' lucid
zinit snippet 'OMZ::plugins/sudo/sudo.plugin.zsh'

zinit ice wait if'(( $+commands[svn] ))' lucid
zinit snippet 'OMZ::plugins/svn/svn.plugin.zsh'

zinit ice wait if'(( $+commands[systemctl] ))' lucid
zinit snippet 'OMZ::plugins/systemd/systemd.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/themes/themes.plugin.zsh'

zinit ice wait svn if'(( $+commands[tmux] ))' lucid
zinit snippet 'OMZ::plugins/tmux'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/torrent/torrent.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/urltools/urltools.plugin.zsh'

zinit ice wait lucid
zinit snippet 'OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh'

zinit ice pick'async.zsh' src'pure.zsh'
zinit light 'sindresorhus/pure'
zstyle ':prompt:pure:execution_time' color yellow
zstyle ':prompt:pure:git:arrow' color cyan
zstyle ':prompt:pure:git:branch' color magenta
zstyle ':prompt:pure:git:branch:cached' color red
zstyle ':prompt:pure:git:action' color yellow
zstyle ':prompt:pure:git:dirty' color default
zstyle ':prompt:pure:host' color yellow
zstyle ':prompt:pure:path' color green
zstyle ':prompt:pure:prompt:error' color red
zstyle ':prompt:pure:prompt:success' color default
zstyle ':prompt:pure:prompt:continuation' color default
zstyle ':prompt:pure:user' color magenta
zstyle ':prompt:pure:user:root' color red
zstyle ':prompt:pure:virtualenv' color default

zinit ice wait lucid
zinit light 'zsh-users/zsh-history-substring-search'

zinit ice wait atload'_zsh_autosuggest_start' lucid
zinit light 'zsh-users/zsh-autosuggestions'

zinit ice wait atload'zicompinit; zicdreplay' blockf lucid
zinit light 'zsh-users/zsh-completions'

zinit ice wait atinit'zpcompinit' lucid
zinit light 'zsh-users/zsh-syntax-highlighting'
