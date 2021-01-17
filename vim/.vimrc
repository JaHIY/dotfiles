" 关闭 vi 兼容模式
set nocompatible

call plug#begin()

Plug 'junegunn/vim-plug'
Plug 'sainnhe/sonokai'
Plug 'sheerun/vim-polyglot'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree' | Plug 'jistr/vim-nerdtree-tabs'
Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'wlangstroth/vim-racket'
Plug 'elixir-lang/vim-elixir'
Plug 'elmcast/elm-vim'
Plug 'lervag/vimtex'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'posva/vim-vue'
Plug 'fsharp/vim-fsharp', { 'for': 'fsharp', 'do':  'make fsautocomplete' }
Plug 'leafgarland/typescript-vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'zah/nim.vim'

call plug#end()
" The caveat is that you should *never* use PlugUpgrade
delc PlugUpgrade

packadd! matchit

" 屏蔽方向键
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" https://vim.fandom.com/wiki/Recover_from_accidental_Ctrl-U
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>

" 显示行号
set number

" 自动语法高亮
syntax on

" 设定配色方案
if has('termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
let g:sonokai_style = 'default'
let g:sonokai_disable_italic_comment = 1
colorscheme sonokai

" 设置右下角标尺
set ruler
set rulerformat=%15(%c%V\ %p%%%)

"用确认对话框（对于 gvim）或命令行选项（对于vim）来代替有未保存内容时的警告信息
set confirm

" 上下可视行数
set scrolloff=6

" 设定 tab 长度为 4
set tabstop=4

" 设定 << 和 >> 命令移动时的宽度为 4
set shiftwidth=4

"开启时，在行首按TAB将加入shiftwidth个空格，否则加入tabstop个空格。
set smarttab

"是否将输入的TAB自动展开成空格。开启后要输入TAB，需要Ctrl-V<TAB>
set expandtab

" 中文帮助
set helplang=cn

" 保留历史记录
set history=1024

" 行控制
set linebreak " 英文单词在换行时不被截断
"set textwidth=80 " 设置每行80个字符自动换行，加上换行符

" 高亮光标所在的行
set cursorline

" 覆盖文件时不备份
set nobackup
set nowb

" 自动切换当前目录为当前文件所在的目录
set autochdir

" 搜索时忽略大小写，但在有一个或以上大写字母时仍大小写敏感
set ignorecase
set smartcase

" 搜索到文件两端时不重新搜索
set nowrapscan

" 实时搜索
set incsearch

" 搜索时高亮显示被找到的文本
set hlsearch

" 关闭错误声音
set noerrorbells
set novisualbell
set t_vb=

" 设定在任何模式下鼠标都可用
set mouse=a

" 自动重新读入
set autoread " 当文件在外部被修改，自动更新该文件

" 检测文件类型
filetype plugin indent on

" 不设定在插入状态无法用退格键和 Delete 键删除回车符
set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" 不自动换行
"set nowrap
"How many tenths of a second to blink
set mat=2

" 允许在有未保存的修改时切换缓冲区，此时的修改由 vim 负责保存
set hidden

" 智能自动缩进
set smartindent

" 设定命令行的行数为 1
set cmdheight=1

" 显示状态栏 (默认值为 1, 无法显示状态栏)
set laststatus=2

" 显示括号配对情况
set showmatch

" （在右下角）显示现有的命令
set showcmd

" 解决自动换行格式下, 如高度在折行之后超过窗口高度结果这一行看不到的问题
set display=lastline

" 设置在状态行显示的信息
set statusline=\ %<%F[%1*%M%*%n%R%H]%=\ %y\ %0(%{&fileformat}\ [%{(&fenc==\"\"?&enc:&fenc).(&bomb?\",BOM\":\"\")}]\ %c:%l/%L%)\ %=\[%P]

" 显示Tab符
set list
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<

" 自动完成
set complete=.,w,b,k,t,i
set completeopt=longest,menu " 只在下拉菜单中显示匹配项目，并且会自动插入所有匹配项目的相同文本

"启动时不显示 捐赠提示
set shortmess=atl

"blank      空白
"buffers    缓冲区
"curdir     当前目录
"folds      折叠
"help       帮助
"options    选项
"tabpages   选项卡
"winsize    窗口大小
"slash      转换文件路径中的\为/以使session文件兼容unix
"unix       设置session文件中的换行模式为unix
set sessionoptions=blank,buffers,curdir,folds,help,options,tabpages,winsize,slash,unix,resize

" {{{ 开始折叠
set foldenable

" 设置语法折叠
" manual  手工定义折叠
" indent  更多的缩进表示更高级别的折叠
" expr    用表达式来定义折叠
" syntax  用语法高亮来定义折叠
" diff    对没有更改的文本进行折叠
" marker  对文中的标志折叠
set foldmethod=indent

"折叠相关的快捷键
"zR 打开所有的折叠
"za Open/Close (toggle) a folded group of lines.
"zA Open a Closed fold or close and open fold recursively.
"zi 全部 展开/关闭 折叠
"zo 打开 (open) 在光标下的折叠
"zc 关闭 (close) 在光标下的折叠
"zC 循环关闭 (Close) 在光标下的所有折叠
"zM 关闭所有可折叠区域
" 设置折叠区域的宽度
set foldcolumn=0

" 设置折叠层数
setlocal foldlevel=1

"设置代码块折叠后显示的行数
set foldexpr=1

" 新建的文件，刚打开的文件不折叠
autocmd! BufNewFile,BufRead * setlocal nofoldenable

" }}}

if has('gui_running')
    if has('win32') || has('win64')
        source $VIMRUNTIME/delmenu.vim
        source $VIMRUNTIME/menu.vim
        set guifont=Fira_Code_Retina:h12:cANSI:qDRAFT
        set guifontwide=Sarasa_Mono_SC:h12:cANSI:qDRAFT
    elseif has('unix')
        set guifont=Fira\ Code\ Retina\ 12
        set guifontwide=Sarasa\ Mono\ SC\ 12
    elseif has('mac')
        set guifont=Monaco:h14
    endif
endif

" {{{ 编码字体设置
set termencoding=utf-8
set fileencodings=ucs-bom,utf-8,shift-jis,cp936,cp950,gb18030,big5,euc-jp,euc-kr,latin1
set fileencoding=utf-8
set ambiwidth=double
" }}}

" nerdtree
let g:NERDTreeWinSize=25
let g:NERDTreeMinimalUI=1
let g:nerdtree_tabs_open_on_gui_startup=0
nmap nc :NERDTreeTabsToggle<cr>

" lightline
set laststatus=2
set noshowmode
let g:lightline = {'colorscheme' : 'sonokai'}

" vimtex
let g:tex_flavor = 'latex'
if has('win32') || has('win64')
    let g:vimtex_view_general_viewer = 'SumatraPDF'
    let g:vimtex_view_general_options_latexmk = '-reuse-instance'
    let g:vimtex_view_general_options
        \ = '-reuse-instance -forward-search @tex @line @pdf'
        \ . ' -inverse-search "' . exepath(v:progpath)
        \ . ' --servername ' . v:servername
        \ . ' --remote-send \"^<C-\^>^<C-n^>'
        \ . ':execute ''drop '' . fnameescape(''\%f'')^<CR^>'
        \ . ':\%l^<CR^>:normal\! zzzv^<CR^>'
        \ . ':call remote_foreground('''.v:servername.''')^<CR^>^<CR^>\""'
elseif has('unix')
    let g:vimtex_view_method = 'zathura'
endif
let g:vimtex_toc_config = {
    \ 'name' : 'TOC',
    \ 'layers' : ['content', 'todo', 'include'],
    \ 'split_width' : 25,
    \ 'todo_sorted' : 0,
    \ 'show_help' : 1,
    \ 'show_numbers' : 1,
    \}
set conceallevel=1
let g:tex_conceal='abdmg'

