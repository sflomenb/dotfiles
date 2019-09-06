syntax on
set number
"set ruler
set autoindent
set autoread
"set bs=2
set incsearch
"set laststatus=2
set backspace=indent,eol,start

filetype indent plugin on

set hls
execute "set <M-h>=˙"
nnoremap <M-h> :noh<CR>

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', ude 4 spaces width
set shiftwidth=4
set softtabstop=4
" On pressing tab, insert 4 spaces
set expandtab
" insert literal tab
inoremap <S-Tab> <C-V><Tab>

fu! SetTab()
    call inputsave()
    let l:resize_amount = input('Enter the number of spaces tabs should be for this file: ')
    call inputrestore()
    call inputsave()
    let l:tab = input('Enter s/t for spaces or tabs: ')
    call inputrestore()
    call inputsave()
    let l:should_retab = input('Enter anything to retab: ')
    call inputrestore()
    if exists("l:resize_amount") && strlen(l:resize_amount) > 0
        if exists("l:tab") && strlen(l:tab) > 0
            if strlen(matchstr(l:tab, '^t')) > 0
                setlocal noexpandtab
            elseif strlen(matchstr(l:tab, '^s')) > 0
                setlocal expandtab
            endif
        endif
        exec 'setlocal tabstop=' . l:resize_amount
        exec 'setlocal shiftwidth=' . l:resize_amount
        exec 'setlocal softtabstop=' . l:resize_amount
        if exists("l:should_retab") && strlen(l:should_retab) > 0
            %retab!
        endif
        "call TabInfo()
    else
        echo 'Clearing tab settings'
        set tabstop<
        set shiftwidth<
        set softtabstop<
        set expandtab<
    endif
endfu

fu! TabInfo()
    set tabstop?
    set shiftwidth?
    set softtabstop?
    set expandtab?
endfu

nnoremap <Leader>t :call TabInfo()<CR>
nnoremap <Leader><Leader>t :call SetTab()<CR>
nnoremap <Leader><Leader><Leader>t :setlocal expandtab!<CR>

cnoremap vres vertical resize
cnoremap bsp below sp
cnoremap vsf vert sf

xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

nnoremap <C-\> :exec 'vertical resize '. string(&columns * 0.25)<CR>


"set showcmd
" macOS clipboard shortcuts
nnoremap <Leader>c :1,$w !pbcopy<CR><CR>
vnoremap <Leader>c y:call system('pbcopy', @")<CR>
"vnoremap <Leader>c :'<,'>w !pbcopy<CR><CR>
nnoremap <Leader>v :r !pbpaste<CR><CR>
vnoremap <Leader>v dk:r !pbpaste<CR>

set nopaste
nnoremap <C-p> :set invpaste paste?<CR>
set pastetoggle=<C-p>
set showmode

set undofile " Maintain history between sessions
set undodir=~/.vim/undodir
if empty(glob('~/.vim/undodir'))
    call mkdir(expand('~/.vim/undodir'))
endif

"set laststatus=2

nnoremap <CR> o<Esc>
nnoremap <C-_> O<Esc>

" plugins
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
else
    call plug#begin('~/.vim/plugged')

    Plug 'airblade/vim-gitgutter'
    Plug 'easymotion/vim-easymotion'
    "Plug 'scrooloose/syntastic'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'tpope/vim-fugitive'
    Plug 'altercation/vim-colors-solarized'
    Plug 'mattn/emmet-vim'
    Plug 'chrisbra/colorizer'
    Plug 'wavded/vim-stylus'
    "Plug 'artur-shaik/vim-javacomplete2'
    "let g:ale_completion_enabled = 1
    "Plug 'w0rp/ale'
    if substitute(system('uname'), '\n', '', '') == 'Darwin'
        Plug 'rizzatti/dash.vim'
    endif
    "Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
    Plug 'neoclide/coc.nvim', { 'do': 'yarn install --frozen-lockfile'}
    Plug 'hashivim/vim-terraform'
    Plug 'rust-lang/rust.vim'
    "Plug 'nlknguyen/cloudformation-syntax.vim'

    "Plug 'prabirshrestha/async.vim'
    "Plug 'prabirshrestha/vim-lsp'
    "Plug 'prabirshrestha/asyncomplete.vim'
    "if executable('pyls')
    "    au User l_setup call l#register_server({
    "        \ 'name': 'pyls',
    "        \ 'cmd': {server_info->['pyls']},
    "        \ 'whitelist': ['python'],
    "        \ })
    "endif

    "CTRL-X CTRL-O

    call plug#end()
    nmap <F6> <Plug>(JavaComplete-Imports-AddMissing)

    " always show signcolumns
    set signcolumn=yes

    " Use tab for trigger completion with characters ahead and navigate.
    " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
    inoremap <silent><expr> <TAB>
                \ pumvisible() ? "\<C-n>" :
                \ <SID>check_back_space() ? "\<TAB>" :
                \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    " Use <c-space> to trigger completion.
    inoremap <silent><expr> <c-space> coc#refresh()
    imap <C-@> <C-Space>

    " use enter to confirm completion
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

    " close preview window when completion is done
    augroup coc
        autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
        autocmd FileType java nnoremap <Leader>o :CocCommand java.action.organizeImports<CR>
    augroup END

    " Remap keys for gotos
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Use K for show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if &filetype == 'vim'
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction

    set cmdheight=2
    nmap <leader>rn <Plug>(coc-rename)

    " Find symbol of current document
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>

    " Use `[c` and `]c` to navigate diagnostics
    nmap <silent> [c <Plug>(coc-diagnostic-prev)
    nmap <silent> ]c <Plug>(coc-diagnostic-next)
    nmap <silent> [e <Plug>(coc-diagnostic-prev-error)
    nmap <silent> ]e <Plug>(coc-diagnostic-next-error)

    " vim-gitgutter bindings
    nmap ]h <Plug>GitGutterNextHunk
    nmap [h <Plug>GitGutterPrevHunk
    let g:gitgutter_diff_args = '-b'


    "let g:ale_linters = {
    "\   'python': [],
    "\}

    " highlight lines git vim-gitgutter
    " let g:gitgutter_highlight_lines = 1

    "map  / <Plug>(easymotion-sn)
    "omap / <Plug>(easymotion-tn)

    " start of default statusline
    "set statusline=%f\ %h%w%m%r\

    " syntastic settings
    "set statusline+=%#warningmsg#
    "set statusline+=%{SyntasticStatuslineFlag()}
    "set statusline+=%*

    " end of default stautsline (with ruler)
    "set statusline+=%=%(%l,%c%V\ %=\ %P%)

    "let g:syntastic_always_populate_loc_list = 1
    "let g:syntastic_auto_loc_list = 1
    "let g:syntastic_check_on_open = 1
    "let g:syntastic_check_on_wq = 0
    "let g:syntastic_aggregate_errors = 1
    "let g:syntastic_html_checkers = ['validator']
endif

syntax enable
try
    colorscheme solarized
    set background=dark
catch /E185/
endtry
    " solarized settings
    "let g:solarized_termcolors=256

" airline theme settings
let g:airline_solarized_bg='dark'

setlocal foldmethod=manual

fu! CommentWord(str)
    exe ':%s/\(\s\(2,}\)\(' . a:str . '\)/1\#/2/gc'
endfu

noremap <Leader>w :call CommentWord("<c-r>=expand("<cword>")<cr>")<CR>

command! FindMergeMarkers /\v(\<{3,}|\={3,}|\>{3,})
noremap <Leader>s :windo set scb!<CR>
command! DT :windo diffthis<CR>
command! DO :windo diffoff<CR>

let g:colorizer_syntax = 1
let g:colorizer_skip_comments = 1

execute "set <M-d>=∂"
noremap <M-d> "_d

nnoremap <C-s> :SyntasticCheck<CR>

nnoremap <Leader>e :Lexplore<CR>
":normal <CR>

fu! SaveSessionAndExit(...)
    set sessionoptions+=globals
    if a:0 && exists("a:1")
        call system("rm !/.vim/sessions/" . g:Save_session_name . ".vim")
        let g:Save_session_name = a:1
    elseif !exists("g:Save_session_name")
        call inputsave()
        let g:Save_session_name = input('Enter session name: ')
        call inputrestore()
    endif
    exe 'mks! ~/.vim/sessions/' . g:Save_session_name . '.vim'
    qa
endfu
nnoremap <Leader><Leader>s :call SaveSessionAndExit()<CR>

command! SS :setlocal spell! spelllang=en_us<CR>

nnoremap <Leader>f :call system('pbcopy', @%)<CR> " copy current file name to clipboard

nnoremap ** *<C-O>:%s///gn<CR>``

let g:netrw_liststyle=3
let g:netrw_banner=0
let g:netrw_altv=1
let g:newtr_winsize=25

fu! Vres(num)
    exec 'vertical resize ' . string(&columns * a:num/100.0)
endfu

fu! Res(num)
    exec 'resize ' . string(&lines * a:num/100.0)
endfu

fu! CallVres()
    call inputsave()
    let l:resize_amount = input('Enter percentage of window that buffer should vertically take up: ')
    call inputrestore()
    call Vres(l:resize_amount)
endfu

nnoremap <Leader>vr :call CallVres()<CR>

fu! CallRes()
    call inputsave()
    let l:resize_amount = input('Enter percentage of window that buffer should vertically take up: ')
    call inputrestore()
    call Res(l:resize_amount)
endfu

nnoremap <Leader>r :call CallRes()<CR>

fu! SwapWindows()
    " if we have already selected a first window
    if exists('g:win_num')
        " store window number of second window
        let l:second_win_num = winnr()

        " get second file name
        let l:second_file = expand('%:p')

        " get first file name
        exec ":norm " .g:win_num . "\<C-w>\C-w>"
        let l:first_file = expand('%:p')

        " open second file in first window
        exec ':e ' . l:second_file

        " switch to second window and open first file
        exec ":norm " .l:second_win_num . "\<C-w>\C-w>"
        exec ':e ' . l:first_file

        " clear the global variable so we can start again
        unlet g:win_num
    else
        " store window number of first window
        let g:win_num = winnr()
        echo g:win_num
    endif
endfu

nnoremap <Leader>sw :call SwapWindows()<CR>

nnoremap <Leader>sc :SyntasticCheck<CR>
nnoremap <Leader>sr :SyntasticReset<CR>


nnoremap <Leader>f : call system('pbcopy', @%)<CR>
nnoremap <Leader>ft : call system('pbcopy', expand('%:t'))<CR>
nnoremap <Leader>ftr : call system('pbcopy', expand('%:t:r'))<CR>

" get current line number
cnoremap <Leader>l =line('.')<CR>

execute 'set <M-l>=¬'
nnoremap <M-l><M-l> :set list!<CR>
try
    set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:_,nbsp:·
catch /E474/
endtry

" filetpye specifc settings
augroup configgroup
    autocmd!
    autocmd BufNewFile,BufRead *.css,*.html,*.htm,*.styl  :ColorHighlight!
    autocmd FileType * setlocal foldmethod=syntax | normal zR
    autocmd FileType python,yaml setlocal foldmethod=indent | normal zR
    autocmd VimEnter * highlight clear SignColumn
    autocmd BufWritePost .vimrc source $MYVIMRC
    autocmd BufReadPost * call TabsOrSpaces()
    autocmd InsertLeave * set nopaste
"    autocmd BufWritePre *.php,*.py,*.js,*.txt,*.hs,*.java,*.md,*.json
"                \ :call <SID>StripTrailingWhitespaces()

    " Uncomment the following to have Vim jump to the last position when
    " reopening a file
    au BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif

    autocmd FileType ruby,javascript setlocal ts=2 sw=2 sts=2
    autocmd CmdwinEnter * nnoremap <CR> <CR>
    autocmd BufReadPost quickfix nnoremap <CR> <CR>
    autocmd FileType java setlocal omnifunc=javacomplete#Complete
    autocmd FileType python nnoremap <buffer> <Leader>m :exec '!python' shellescape(@%, 1)<CR>
    autocmd FileType cpp nnoremap <buffer> <Leader>m :exec '!make'<CR>
    autocmd FileType python nnoremap <buffer> <Leader><Leader>m :exec '!python3' shellescape(@%, 1)<CR>
    autocmd FileType netrw noremap gn :Ntree <CR>

    " python logging
    autocmd FileType python inoremap <M-l> logging.debug()<Esc>F(a
    autocmd FileType python nnoremap <M-l> yiwologging.debug()<Esc>F(a'<Esc>pa: %s', <Esc>p
    autocmd FileType python vnoremap <M-l> yologging.debug()<Esc>F(a'<Esc>pa: %s', <Esc>p
    " python debugging
    autocmd FileType python nnoremap <M-d> oimport pdb; pdb.set_trace()<Esc>
    autocmd FileType python inoremap <M-d> <Esc>oimport pdb; pdb.set_trace()A
    " java logging
    autocmd FileType java inoremap <M-l> System.out.println()<Esc>F(a
    autocmd FileType java nnoremap <M-l> yiwoSystem.out.println()<Esc>F(a"<Esc>pa: " + <Esc>pA;<Esc>
    autocmd FileType java vnoremap <M-l> yoSystem.out.println()<Esc>F(a"<Esc>pa: " + <Esc>pA;<Esc>
    " javascript debugging
    autocmd FileType javascript nnoremap <M-d> odebugger;<Esc>
    autocmd FileType javascript inoremap <M-d> <Esc>odebugger;A

    autocmd BufWinEnter * highlight ColorColumn ctermbg=magenta
    autocmd BufWinEnter * call matchadd('ColorColumn', '\%81v', 100)
augroup END

" toggle between number and relativenumber
function! ToggleNumber()
    if(&relativenumber == 1)
        set norelativenumber
        set number
    else
        set relativenumber
    endif
endfunc

"execute 'set <M-n>=˜'
nnoremap <Leader>n :call ToggleNumber()<CR>

" strips trailing whitespace at the end of files. this
" is called on buffer write in the autogroup above.
function! StripTrailingWhitespaces()
    " save last search & cursor position
    let _s=@/
    let l = line(".")
    let c = col(".")
    execute a:firstline . ',' . a:lastline . 's/\s\+$//e'
    let @/=_s
    call cursor(l, c)
endfunction

" make StripTrailingWhitespaces a command that take a range
command! -range=% RW <line1>,<line2>call StripTrailingWhitespaces()

nnoremap<Leader>" viw<Esc>a"<Esc>bi"<Esc>lel
nnoremap<Leader>' viw<Esc>a'<Esc>bi'<Esc>lel

nnoremap gV `[v`]

function! TabsOrSpaces()
    " Determines whether to use spaces or tabs on the current buffer.
    if getfsize(bufname("%")) > 256000
        " File is very large, just use the default.
        return
    endif

    let numTabs=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^\\t"'))
    let numSpaces=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^ "'))

    if numTabs > numSpaces
        setlocal noexpandtab
        exec 'setlocal tabstop=' . &tabstop
        exec 'setlocal shiftwidth=' . &tabstop
        exec 'setlocal softtabstop=' . &tabstop
    endif
endfunction

fu! VisualSearch()
    try
        let l:a_save = @a
        silent! normal gv"ay
        let l:search_val = @a
        "echom l:search_val
        "let l:search_val = substitute(l:search_val, '\(/\|\.\|\[\|\]\)', '\\\0', 'g')
        let l:search_val = escape(l:search_val, '/\.*$[]')
        let @/ = l:search_val
    finally
        let @a = a_save
    endtry
endfu

vnoremap * :call VisualSearch()<CR>
vnoremap ** :call VisualSearch()<CR>:%s///gn<CR>``

fu! EscapeSearch()
    call inputsave()
    let l:text_to_search = input('Search term: ')
    call inputrestore()
    let l:search_val = l:text_to_search
    let l:search_val = escape(l:search_val, '/\.*$[]')
    let @/ = l:search_val
endfu

nnoremap // :call EscapeSearch()<CR>

nnoremap <Leader>vrc :tabe $MYVIMRC<CR>

function! GetYamlIndent()
    let lnum = line('.') - 1
    let indent = indent(lnum)
    let increase = indent + &sw
    return indent
endfunction

" change cursor in indent mode
let &t_SI = "\e[5 q"
let &t_EI = "\e[2 q"

augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
augroup END

" Show trailing whitespace and spaces before a tab
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

function! SubstituteKeepCase(input)
    let l:search_terms = split(a:input, '/')
    if len(l:search_terms) < 2 || len(l:search_terms) > 3
        echom "Please use syntax ':S/<find>/<replace>/<options>'"
    else
        " save last match & cursor position
        let _s=@/
        let l = line(".")
        let c = col(".")

        " main work
        " check if additional search options were passed in
        let l:search_options = ''
        if len(l:search_terms) == 3
            let l:search_options = l:search_terms[2]
        endif
        let l:find_term = l:search_terms[0]
        let l:replace_term = l:search_terms[1]
        let l:replace_first_letter_lower = l:replace_term[0]
        let l:replace_first_letter_upper = toupper(l:replace_first_letter_lower)
        let l:replace_rest_lower = l:replace_term[1:]
        let l:replace_rest_upper = toupper(l:replace_rest_lower)
        execute a:firstline . ',' . a:lastline . 's/\c\(' . l:find_term . '\)' . "/\\= ( submatch(0)[0] is# toupper(submatch(0)[0]) ? '" . l:replace_first_letter_upper . "' : '" . l:replace_first_letter_lower . "' ) . ( submatch(0)[1] is# toupper(submatch(0)[1]) ? '" . l:replace_rest_upper . "' : '" . l:replace_rest_lower . "' )/" . l:search_options

        " restore current position and search
        let @/=_s
        call cursor(l, c)
    endif
endfunction

command! -range -nargs=* S <line1>,<line2>call SubstituteKeepCase(<f-args>)

function! ReplaceAllCamelCaseToSnakeCase()
    " save search
    let _s=@/
    let l = line(".")
    let c = col(".")

    let l:matches = []
    execute ':keeppatterns %s/' . '\<[a-z]\+\([A-Z][a-z]*\)\+\>' . '/\=add(l:matches,submatch(0))/gn'
    call uniq(sort(l:matches))
    for str in l:matches
        call CamelCaseToSnakeCase(str)
    endfor

    " restore current position and search
    let @/=_s
    call cursor(l, c)
endfunction

function! CamelCaseToSnakeCase(match)
    let l:words = split(a:match, '[A-Z]\zs')
    let l:new_string = ''
    for word in l:words
        let l:beginning = word[:-2]
        let l:last_letter = word[-1:]
        let l:last_letter_uppercase = toupper(l:last_letter)
        if l:last_letter is l:last_letter_uppercase
            let l:new_string = l:new_string . l:beginning . '_' . tolower(l:last_letter)
        else
            let l:new_string = l:new_string . l:word
        endif
    endfor
    execute ':keeppatterns %s/' . a:match . '/' . l:new_string . '/gc'
endfunction

command! CamelCaseToSnakeCase call ReplaceAllCamelCaseToSnakeCase()

set breakindent
set hidden

" add matching brace characters
"inoremap { {}<C-G>U<Left>
"inoremap ( ()<C-G>U<Left>
"inoremap [ []<C-G>U<Left>
"inoremap < <><C-G>U<Left>
"inoremap ' ''<C-G>U<Left>
""inoremap <expr> ' strpart(getline('.'), col('.')-1, 1) == "\'" ? "\<Right>" : "\'\'\<Left>"
"inoremap " ""<C-G>U<Left>

" native fuzzy find
set path+=**
set wildignore+=**/target/**,**/*.class,**/*.pyc
set wildmenu

au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

