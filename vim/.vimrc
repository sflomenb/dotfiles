syntax on
set number
set ruler
set autoread
set bs=2
set incsearch
set laststatus=2
set backspace=indent,eol,start

set smartcase
set ignorecase

filetype indent plugin on

set hls
execute "set <M-h>=˙"
nnoremap <M-h> :noh<CR>

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

" Maintain history between sessions
set undofile
for dir in ['.undo', '.backup', '.swp']
    if empty(glob('~/.vim/' . dir)) && exists("*mkdir")
        call mkdir($HOME . '/.vim/' . dir)
    endif
endfor
set undodir=~/.vim/.undo//
set backupdir=~/.vim/.backup//
set directory=~/.vim/.swp//

"set laststatus=2

inoremap <C-_> <C-o>O

function! FileMatchesRegex(filename, regex)
    if filereadable(a:filename)
        for line in readfile(a:filename)
            if line =~ escape(a:regex, '/\.*$^~[]()|')
                return 0
            endif
        endfor
    endif
    return 1
endfunction

" plugins
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
elseif &loadplugins
    call plug#begin('~/.vim/plugged')

    Plug 'airblade/vim-gitgutter'
    Plug 'easymotion/vim-easymotion'
    Plug 'tpope/vim-fugitive'
    Plug 'altercation/vim-colors-solarized'
    Plug 'mattn/emmet-vim'
    Plug 'chrisbra/colorizer'
    Plug 'wavded/vim-stylus'
    " install Dash only on Mac
    if substitute(system('uname'), '\n', '', '') == 'Darwin'
        Plug 'rizzatti/dash.vim'
    endif
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'hashivim/vim-terraform'
    Plug 'janko/vim-test'
    Plug 'rust-lang/rust.vim'
    Plug 'posva/vim-vue'
    Plug 'fatih/vim-go'
    Plug 'tpope/vim-surround'
    Plug 'dense-analysis/ale'
    Plug 'hynek/vim-python-pep8-indent'
    Plug 'chr4/nginx.vim'
    Plug 'morhetz/gruvbox'

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
        autocmd FileType python nnoremap <Leader>o :CocCommand python.sortImports<CR>
        " Highlight symbol under cursor on CursorHold
        autocmd CursorHold * :silent call CocActionAsync('highlight')
    augroup END

    " Remap keys for gotos
    nmap <silent> gcd <Plug>(coc-definition)
    nmap <silent> gcy <Plug>(coc-type-definition)
    nmap <silent> gci <Plug>(coc-implementation)
    nmap <silent> gcr <Plug>(coc-references)

    " Use K for show documentation in preview window
    nnoremap <silent> Kc :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if &filetype == 'vim'
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction

    set cmdheight=2
    set shortmess+=c
    nmap <leader>rn <Plug>(coc-rename)

    " Find symbol of current document
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>

    " Use `[g` and `]g` to navigate diagnostics
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)
    nmap <silent> [e <Plug>(coc-diagnostic-prev-error)
    nmap <silent> ]e <Plug>(coc-diagnostic-next-error)

    " command for disabling coc term transparency
    fu! ToggleTransparencyFun()
        if (!exists("g:solarized_termtrans")) || g:solarized_termtrans == 0
            let g:solarized_termtrans = 1
        else
            let g:solarized_termtrans = 0
        endif
    endfu
    command! ToggleTransparency :call ToggleTransparencyFun() | source $MYVIMRC

    " vim-gitgutter bindings
    nmap ]h <Plug>(GitGutterNextHunk)
    nmap [h <Plug>(GitGutterPrevHunk)
    let g:gitgutter_diff_args = '-b'


    " vim-test mappings
    nmap <silent> t<C-n> :TestNearest<CR>
    nmap <silent> t<C-f> :TestFile<CR>
    nmap <silent> t<C-s> :TestSuite<CR>
    nmap <silent> t<C-l> :TestLast<CR>
    nmap <silent> t<C-g> :TestVisit<CR>
    let test#java#maventest#options = '-DtrimStackTrace=false'
    if FileMatchesRegex('package.json', '^\s*"\zstest:unit\ze')
        let test#javascript#jest#executable = 'yarn test:unit'
    endif

    " ale mappings
    nmap <silent> [a <Plug>(ale_previous_wrap)
    nmap <silent> ]a <Plug>(ale_next_wrap)
    let g:ale_echo_msg_error_str = 'E'
    let g:ale_echo_msg_warning_str = 'W'
    let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
    let g:ale_python_pylint_options = '--load-plugins=pylint_django --disable=C0114,C0115,C0116'

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
    if $TERM_PROGRAM !=? 'apple_terminal'
        if $TERM_PROGRAM ==# 'iTerm.app'
            let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
            let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
        endif
        set termguicolors
    endif
    colorscheme gruvbox
catch /E185/
endtry

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

command! SS :setlocal spell! spelllang=en_us

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

set encoding=utf-8
scriptencoding utf-8
execute 'set <M-l>=¬'
nnoremap <M-l><M-l> :set list!<CR>
try
    set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:_,nbsp:·
catch /E474/
endtry

function! PythonLogging(first_part, second_part)
    let l:to_insert = ''
    let l:second_part = a:second_part
    if search('import logging', 'nw')
        let l:to_insert = 'logging.debug()'
    else
        let l:to_insert = 'print()'
        let l:second_part = substitute(l:second_part, "%s',", "' + ", '')
    endif
    return a:first_part . l:to_insert . l:second_part
endfunction

" filetpye specifc settings
augroup configgroup
    autocmd!
    autocmd BufNewFile,BufRead *.css,*.html,*.htm,*.styl  :ColorHighlight!
    autocmd VimEnter * highlight clear SignColumn
    autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
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

    autocmd FileType * setl ts=4 sw=4 sts=4 et ai si
    autocmd FileType ruby,javascript,css,stylus,vue setlocal ts=2 sw=2 sts=2
    autocmd FileType yaml setlocal ts=2 sw=2 sts=2
        \ indentexpr=GetYamlIndent()
        \ indentkeys-=0#
        \ indentkeys-=<:>
    autocmd FileType java setlocal omnifunc=javacomplete#Complete
    autocmd FileType python nnoremap <buffer> <Leader>m :exec '!python' shellescape(@%, 1)<CR>
    autocmd FileType cpp nnoremap <buffer> <Leader>m :exec '!make'<CR>
    autocmd FileType python nnoremap <buffer> <Leader><Leader>m :exec '!python3' shellescape(@%, 1)<CR>

    " python logging
    autocmd FileType python inoremap <expr> <M-l> PythonLogging('', '<Esc>F(a')
    autocmd FileType python nnoremap <expr> <M-l> PythonLogging('yiwo', "<Esc>F(a'<Esc>pa: %s', <Esc>p")
    autocmd FileType python vnoremap <expr> <M-l> PythonLogging('yo', "<Esc>F(a'<Esc>pa: %s', <Esc>p")
    " python debugging
    autocmd FileType python nnoremap <M-d> oimport pdb; pdb.set_trace()<Esc>
    autocmd FileType python inoremap <M-d> <Esc>oimport pdb; pdb.set_trace()A
    " java logging
    autocmd FileType java inoremap <M-l> System.out.println();<Esc>F(a
    autocmd FileType java nnoremap <M-l> yiwoSystem.out.println()<Esc>F(a"<Esc>pa: " + <Esc>pA;<Esc>
    autocmd FileType java vnoremap <M-l> yoSystem.out.println()<Esc>F(a"<Esc>pa: " + <Esc>pA;<Esc>
    " javascript logging
    autocmd FileType javascript,vue inoremap <M-l> console.log()<Esc>F(a
    autocmd FileType javascript,vue nnoremap <M-l> yiwoconsole.log()<Esc>F(a"<Esc>pa: ", <Esc>pA<Esc>
    autocmd FileType javascript,vue vnoremap <M-l> yoconsole.log()<Esc>F(a"<Esc>pa: ", <Esc>pA<Esc>
    " javascript debugging
    autocmd FileType javascript nnoremap <M-d> odebugger;<Esc>
    autocmd FileType javascript inoremap <M-d> <Esc>odebugger;A
    " go logging
    autocmd FileType go inoremap <M-l> fmt.Println()<Esc>F(a
    autocmd FileType go nnoremap <M-l> yiwofmt.Println()<Esc>F(a"<Esc>pa: ", <Esc>p
    autocmd FileType go vnoremap <M-l> yofmt.Println()<Esc>F(a"<Esc>pa: ", <Esc>p
    " rust logging
    autocmd FileType rust inoremap <M-l> println!()<Esc>F(a
    autocmd FileType rust nnoremap <M-l> yiwoprintln!()<Esc>F(a"<Esc>pa: {:?}", <Esc>pA;<Esc>
    autocmd FileType rust vnoremap <M-l> yoprintln!()<Esc>F(a"<Esc>pa: {:?}", <Esc>pA;<Esc>

    " fold settings by language
    autocmd FileType * setlocal foldmethod=syntax | normal zR
    autocmd FileType python,yaml setlocal foldmethod=indent | normal zR
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
function! StripTrailingWhitespaces() range
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

fu! ConvertNumSpaces()
    call inputsave()
    let l:current_amount = input('Enter current number of spaces: ')
    call inputrestore()
    call inputsave()
    let l:desired_amount = input('Enter desired number of spaces: ')
    call inputrestore()
    exec 'set ts=' . l:current_amount . ' sts=' . l:current_amount . ' noet'
    retab!
    exec 'set ts=' . l:desired_amount . ' sts=' . l:desired_amount . ' et'
    retab
endfu

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
augroup whitespace
    autocmd!
    autocmd BufWinEnter,InsertLeave,ColorScheme,ColorSchemePre * highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
    autocmd BufWinEnter,InsertLeave,ColorScheme,ColorSchemePre * match ExtraWhitespace /\s\+$/
    autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    autocmd BufWinLeave * call clearmatches()
augroup END

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
set wildignore+=**/target/**,**/*.class,**/*.pyc,**/*.o,**/node_modules/**
set wildmenu

au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
au FileType gitcommit setlocal spell

runtime macros/matchit.vim
" Matchit support:
if exists("loaded_matchit")
    if !exists("b:match_words")
        let b:match_ignorecase = 0
        let b:match_words =
            \ '\%(\%(\%(^\|[;=]\)\s*\)\@<=\%(class\|module\|while\|begin\|until\|for\|if\|unless\|def\|case\)\|\<do\)\>:' .
            \ '\<\%(else\|elsif\|ensure\|rescue\|when\)\>:\%(^\|[^.]\)\@<=\<end\>'
    endif
endif

augroup column
    autocmd WinEnter,BufEnter,InsertChange,ColorScheme,ColorSchemePre * highlight ColorColumn ctermbg=magenta
    autocmd WinEnter,BufEnter,InsertChange,ColorScheme,ColorSchemePre * call matchadd('ColorColumn', '\%81v', 100)
    autocmd WinLeave * call clearmatches()
augroup END

set autoread
augroup file
    autocmd CursorHold * if expand('%') !=# '[Command Line]' | checktime | endif
augroup END

fu! s:rename(new_name)
    let l:old_name = expand('%')
    let l:old_name_abs_path = expand('%:p')
    let l:old_undo_file = substitute(expand(&undodir), '//', '/', '') . substitute(l:old_name_abs_path, '/', '%', 'g')
    let l:choice = confirm('Rename ' . expand('%') . ' to: ' . a:new_name . '. Continue? ', "&Yes\n&No", 2)
    echo(l:choice)
    if l:choice == 1
        echo "Renaming"
        try
            if a:new_name == l:old_name
                throw 'File names are the same'
            else
                echo('Renaming')
                " save copy with new name
                exec 'saveas ' . a:new_name
                " delete old file
                call delete(fnameescape(l:old_name_abs_path))
                " delete old undofile
                call delete(fnameescape(l:old_undo_file))
            endif
        catch
            echo v:exception
        endtry
    else
        echo "Not renaming"
    endif
endfu
command! -nargs=1 -complete=file Rename call <SID>rename(<f-args>)

function! GetSelectedText()
    normal gv"xy
    let l:result = getreg("x")
    normal gv
    return l:result
endfunction

function! EscapeText(str)
    let l:escaped = escape(a:str, '/\.*$^~[]()|')
    let @x = l:escaped
    norm "xp
endfunction

function! GitBranch()
    if &ft !~ '^\(git\|diff\)' && empty(glob('.git/rebase'))
        silent return system("git rev-parse --abbrev-ref HEAD 2> /dev/null | tr -d '\n'")
    else
        return ''
    endif
endfunction

function! StatuslineGit()
    let l:branchname = GitBranch()
    let l:ratio = winwidth(0) / len(l:branchname)
    if l:ratio <= 2
        " shortern branch name
        let l:list = matchlist(l:branchname, '^.*\/\([A-Z0-9]*-[0-9]*\)')
        if len(l:list) >= 2 && !empty(l:list[1])
            let l:branchname = l:list[1]
        endif
    endif
    return !empty(l:branchname) ? '  ' . l:branchname . ' ' : ''
endfunction

function! StatusFilename()
    let l:pre = ''
    let l:pat = '://'
    let l:name = expand('%:~:.')
    if l:name =~# l:pat
        let l:pre = l:name[:stridx(l:name, l:pat) + len(l:pat) - 1] . '...'
        let l:name = l:name[stridx(l:name, l:pat) + len(l:pat):]
    elseif empty(l:name) && &filetype ==# 'netrw'
        let l:name = fnamemodify(b:netrw_curdir, ':~:. . '...'')
    endif
    let l:name = simplify(l:name)
    let l:ratio = winwidth(0) / len(l:name)
    if l:ratio <= 2 && l:ratio > 1
        let l:name = pathshorten(l:name)
    elseif l:ratio <= 1
        let l:name = fnamemodify(l:name, ':t')
    endif
    return l:pre  . l:name
endfunction

function! ActiveStatus()
    let statusline=""
    let statusline.="%#PmenuThumb#"
    let statusline.="%(%{StatuslineGit()}%)"
    let statusline.="%*"
    let statusline.="\ %{StatusFilename()}"
    let statusline.="%m"
    let statusline.="%r"
    let statusline.="%="
    let statusline.="\ %y"
    let statusline.="\ %{&fileencoding?&fileencoding:&encoding}"
    let statusline.="\[%{&fileformat}\]"
    let statusline.="\ %p%%"
    let statusline.="\ %l:%c"
    let statusline.="\ #%{winnr()}"
    let statusline.="\ "
    return statusline
endfunction

function! InactiveStatus()
    let statusline=""
    "let statusline.="%#PmenuSel#"
    "let statusline.="%(%{StatuslineGit()}%)"
    "let statusline.="%*"
    let statusline.="\ %{StatusFilename()}"
    let statusline.="%m"
    let statusline.="%r"
    let statusline.="%="
    "let statusline.="\ %y"
    "let statusline.="\ %{&fileencoding?&fileencoding:&encoding}"
    "let statusline.="\[%{&fileformat}\]"
    "let statusline.="\ %p%%"
    "let statusline.="\ %l:%c"
    let statusline.="\ #%{winnr()}"
    let statusline.="\ "
    return statusline
endfunction

set statusline=%!InactiveStatus()

augroup statusline
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!ActiveStatus()
    autocmd WinLeave * setlocal statusline=%!InactiveStatus()
augroup END

fu! BlockCommentFolds()
    let l:thisline  = getline(v:lnum)
    if match(l:thisline, '/\*') >= 0
        return ">1"
    elseif match(l:thisline, '\*/') >= 0
        return "<1"
    else
        return "="
    endif
endfu

fu! FoldBlockComments()
    if &foldmethod != 'expr'
        setl foldmethod=expr
        setl foldexpr=BlockCommentFolds()
        norm zM
    else
        set foldmethod<
        set foldexpr<
        norm zR
    endif
    set foldmethod?
    set foldexpr?
endfu

command! FoldBlockComments :call FoldBlockComments()

set updatetime=300

function! SetBackgroundMode(...)
    let l:new_bg = "dark"
    if system('uname') =~? "Darwin"
        silent let l:cur_bg = system("osascript ~/light-dark.scpt")
        if l:cur_bg =~? "dark"
            let l:new_bg = "dark"
        elseif l:cur_bg =~? "light"
            let l:new_bg = "light"
        endif
    else
        " This is for Linux where I use an environment variable for this:
        if $VIM_BACKGROUND ==? "dark"
            let l:new_bg = "dark"
        else
            let l:new_bg = "light"
        endif
    endif
    if &background !=? l:new_bg
        let &background = l:new_bg
    endif
endfunction
call SetBackgroundMode()
call timer_start(3000, "SetBackgroundMode", {"repeat": -1})

