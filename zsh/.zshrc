# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

#zinit
source "$HOME/.zinit/bin/zinit.zsh"

zinit snippet 'OMZ::lib/bzr.zsh'
zinit snippet 'OMZ::lib/clipboard.zsh'
zinit snippet 'OMZ::lib/compfix.zsh'
zinit snippet 'OMZ::lib/completion.zsh'
zinit snippet 'OMZ::lib/correction.zsh'
zinit snippet 'OMZ::lib/diagnostics.zsh'
zinit snippet 'OMZ::lib/directories.zsh'
zinit snippet 'OMZ::lib/functions.zsh'
zinit snippet 'OMZ::lib/git.zsh'
zinit snippet 'OMZ::lib/grep.zsh'
zinit snippet 'OMZ::lib/history.zsh'
zinit snippet 'OMZ::lib/key-bindings.zsh'
zinit snippet 'OMZ::lib/misc.zsh'
zinit snippet 'OMZ::lib/nvm.zsh'
zinit snippet 'OMZ::lib/prompt_info_functions.zsh'
zinit snippet 'OMZ::lib/spectrum.zsh'
zinit snippet 'OMZ::lib/termsupport.zsh'
zinit snippet 'OMZ::lib/theme-and-appearance.zsh'

zinit ice as"completion" if'(( $+commands[adb] ))'
zinit snippet 'OMZ::plugins/adb/_adb'

zinit ice if'(( $+commands[ant] ))'
zinit snippet 'OMZ::plugins/ant/ant.plugin.zsh'

zinit ice if"grep -s -q -F 'Arch Linux' /etc/os-release"
zinit snippet 'OMZ::plugins/archlinux/archlinux.plugin.zsh'

zinit ice if'(( $+commands[convert] ))'
zinit snippet 'OMZ::plugins/catimg/catimg.plugin.zsh'

zinit snippet 'OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh'
zinit snippet 'OMZ::plugins/command-not-found/command-not-found.plugin.zsh'
zinit snippet 'OMZ::plugins/common-aliases/common-aliases.plugin.zsh'
zinit snippet 'OMZ::plugins/copydir/copydir.plugin.zsh'
zinit snippet 'OMZ::plugins/copyfile/copyfile.plugin.zsh'
zinit snippet 'OMZ::plugins/cp/cp.plugin.zsh'

zinit ice if'(( $+commands[cpanm] ))'
zinit snippet 'OMZ::plugins/cpanm/cpanm.plugin.zsh'

zinit snippet 'OMZ::plugins/encode64/encode64.plugin.zsh'
zinit snippet 'OMZ::plugins/extract/extract.plugin.zsh'

zinit ice if'(( $+commands[git] ))'
zinit snippet 'OMZ::plugins/git/git.plugin.zsh'

zinit snippet 'OMZ::plugins/gnu-utils/gnu-utils.plugin.zsh'

zinit ice if'(( $+commands[go] ))'
zinit snippet 'OMZ::plugins/golang/golang.plugin.zsh'

zinit ice if'(( $+commands[gradle] ))'
zinit snippet 'OMZ::plugins/gradle/gradle.plugin.zsh'

zinit ice if'(( $+commands[httpie] ))'
zinit snippet 'OMZ::plugins/httpie/httpie.plugin.zsh'

zinit snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'

zinit ice as"completion" if'(( $+commands[lein] ))'
zinit snippet 'OMZ::plugins/lein/_lein'

zinit ice if'(( $+commands[hg] ))'
zinit snippet 'OMZ::plugins/mercurial/mercurial.plugin.zsh'

zinit ice if'(( $+commands[npm] ))'
zinit snippet 'OMZ::plugins/npm/npm.plugin.zsh'

zinit ice if'(( $+commands[perl] ))'
zinit snippet 'OMZ::plugins/perl/perl.plugin.zsh'

zinit ice if'(( $+commands[python] ))'
zinit snippet 'OMZ::plugins/python/python.plugin.zsh'

zinit ice if'(( $+commands[rebar] ))'
zinit snippet 'OMZ::plugins/rebar/rebar.plugin.zsh'

zinit ice if'(( $+commands[rsync] ))'
zinit snippet 'OMZ::plugins/rsync/rsync.plugin.zsh'

zinit ice if'(( $+commands[ruby] ))'
zinit snippet 'OMZ::plugins/ruby/ruby.plugin.zsh'

zinit ice as"completion" if'(( $+commands[rustc] ))'
zinit snippet 'OMZ::plugins/rust/_rust'

zinit ice if'(( $+commands[rvm] ))'
zinit snippet 'OMZ::plugins/rvm/rvm.plugin.zsh'

zinit ice if'(( $+commands[sbt] ))'
zinit snippet 'OMZ::plugins/sbt/sbt.plugin.zsh'

zinit ice as"completion" if'(( $+commands[scala] ))'
zinit snippet 'OMZ::plugins/scala/_scala'

zinit ice if'(( $+commands[screen] ))'
zinit snippet 'OMZ::plugins/screen/screen.plugin.zsh'

zinit ice if'(( $+commands[sudo] ))'
zinit snippet 'OMZ::plugins/sudo/sudo.plugin.zsh'

zinit ice if'(( $+commands[svn] ))'
zinit snippet 'OMZ::plugins/svn/svn.plugin.zsh'

zinit ice if'(( $+commands[systemctl] ))'
zinit snippet 'OMZ::plugins/systemd/systemd.plugin.zsh'

zinit snippet 'OMZ::plugins/themes/themes.plugin.zsh'

zinit ice svn if'(( $+commands[tmux] ))'
zinit snippet 'OMZ::plugins/tmux'

zinit snippet 'OMZ::plugins/torrent/torrent.plugin.zsh'
zinit snippet 'OMZ::plugins/urltools/urltools.plugin.zsh'
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

zinit light 'zsh-users/zsh-history-substring-search'
zinit light 'zsh-users/zsh-autosuggestions'
zinit light 'zsh-users/zsh-completions'
zinit light 'zsh-users/zsh-syntax-highlighting'
