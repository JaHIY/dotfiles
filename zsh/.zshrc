# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

#zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
source "${ZINIT_HOME}/zinit.zsh"

setopt promptsubst

zinit for OMZ::lib/{bzr,clipboard,compfix,completion,correction,diagnostics,directories,functions,git,grep,history,key-bindings,misc,nvm,prompt_info_functions,spectrum,termsupport,theme-and-appearance}.zsh

zinit ice wait if"grep -s -q -F 'Arch Linux' /etc/os-release" lucid
zinit snippet 'OMZP::archlinux'

zinit ice wait lucid
zinit snippet 'OMZP::colored-man-pages'

zinit ice wait lucid
zinit snippet 'OMZP::command-not-found'

zinit ice wait lucid
zinit snippet 'OMZP::common-aliases'

zinit ice wait lucid
zinit snippet 'OMZP::cp'

zinit ice wait if'(( $+commands[cpanm] ))' lucid
zinit snippet 'OMZP::cpanm'

zinit ice wait lucid
zinit snippet 'OMZP::encode64'

zinit ice wait lucid
zinit snippet 'OMZP::extract'

zinit ice wait if'(( $+commands[git] ))' lucid
zinit snippet 'OMZP::git'

zinit ice wait lucid
zinit snippet 'OMZP::gnu-utils'

zinit ice wait if'(( $+commands[go] ))' lucid
zinit snippet 'OMZP::golang'

zinit ice wait lucid
zinit snippet 'OMZP::jsontools'

zinit ice wait if'(( $+commands[hg] ))' lucid
zinit snippet 'OMZP::mercurial'

zinit ice wait if'(( $+commands[npm] ))' lucid
zinit snippet 'OMZP::npm'

zinit ice wait if'(( $+commands[perl] ))' lucid
zinit snippet 'OMZP::perl'

zinit ice wait if'(( $+commands[python] ))' lucid
zinit snippet 'OMZP::python'

zinit ice wait if'(( $+commands[rsync] ))' lucid
zinit snippet 'OMZP::rsync'

zinit ice wait if'(( $+commands[ruby] ))' lucid
zinit snippet 'OMZP::ruby'

zinit ice wait if'(( $+commands[screen] ))' lucid
zinit snippet 'OMZP::screen'

zinit ice wait if'(( $+commands[sudo] ))' lucid
zinit snippet 'OMZP::sudo'

zinit ice wait if'(( $+commands[svn] ))' lucid
zinit snippet 'OMZP::svn'

zinit ice wait if'(( $+commands[systemctl] ))' lucid
zinit snippet 'OMZP::systemd'

zinit ice wait lucid
zinit snippet 'OMZP::themes'

zinit ice wait lucid
zinit snippet 'OMZP::torrent'

zinit ice wait lucid
zinit snippet 'OMZP::urltools'

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

zinit wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    'zdharma-continuum/fast-syntax-highlighting' \
    blockf \
    'zsh-users/zsh-completions' \
    atload"!_zsh_autosuggest_start" \
    'zsh-users/zsh-autosuggestions'
