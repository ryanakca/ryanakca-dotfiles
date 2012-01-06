" p
" Line numbering
"set number
" Set syntax highlighting
syntax on
colorscheme brookstream
" Tabbing
set et
set shiftwidth=4
set smarttab
set autoindent
" We want a mouse in consoles, so:"
set mouse=a
filetype plugin on
filetype indent on
set textwidth=80
set bg=dark
set modeline
set modelines=1
command -nargs=1 Hugged :/<args>/s/\(.*\) ;\(.*\)\(||\)/\1 lightgreen;\2 RyanKavanagh ||/c
""" Don't connect to X clipboard, slow
""" set clipboard=exclude:.*
" For :sp, switching and minimum height
" map <C-H> <C-W>j<C-W>_
" map <C-L> <C-W>k<C-W>_
" set wmh=0
" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on
" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to alway generate a file-name.
set grepprg=grep\ -nH\ $*

" Haskell crud
au BufEnter *.hs compiler ghc
let g:haddock_browser = "/usr/bin/w3m"
let g:ghc_version = 6.8.2
let g:ghc = "/usr/bin/ghc"

" Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff <wouter@blub.net>
augroup encrypted
au!
" First make sure nothing is written to ~/.viminfo while editing
" an encrypted file.
autocmd BufReadPre,FileReadPre      *.gpg set viminfo=
" We don't want a swap file, as it writes unencrypted data to disk
autocmd BufReadPre,FileReadPre      *.gpg set noswapfile
" Switch to binary mode to read the encrypted file
autocmd BufReadPre,FileReadPre      *.gpg set bin
autocmd BufReadPre,FileReadPre      *.gpg let ch_save = &ch|set ch=2
autocmd BufReadPre,FileReadPre      *.gpg let shsave=&sh
autocmd BufReadPre,FileReadPre      *.gpg let &sh='sh'
autocmd BufReadPre,FileReadPre      *.gpg let ch_save = &ch|set ch=2
autocmd BufReadPost,FileReadPost    *.gpg '[,']!gpg --decrypt --default-recipient-self 2> /dev/null
autocmd BufReadPost,FileReadPost    *.gpg let &sh=shsave
" Switch to normal mode for editing
autocmd BufReadPost,FileReadPost    *.gpg set nobin
autocmd BufReadPost,FileReadPost    *.gpg let &ch = ch_save|unlet ch_save
autocmd BufReadPost,FileReadPost    *.gpg execute ":doautocmd BufReadPost " . expand("%:r")
" Convert all text to encrypted text before writing
autocmd BufWritePre,FileWritePre    *.gpg set bin
autocmd BufWritePre,FileWritePre    *.gpg let shsave=&sh
autocmd BufWritePre,FileWritePre    *.gpg let &sh='sh'
autocmd BufWritePre,FileWritePre    *.gpg '[,']!gpg --encrypt --default-recipient-self 2>/dev/null
autocmd BufWritePre,FileWritePre    *.gpg let &sh=shsave
" Undo the encryption so we are back in the normal text, directly
" after the file has been written.
autocmd BufWritePost,FileWritePost  *.gpg   silent u
autocmd BufWritePost,FileWritePost  *.gpg set nobin
augroup END

" Support my daylog
au BufNewFile,BufRead daylog,daylog.dch setf debchangelog
au BufNewFile,BufRead *.tikz setf tex

map! i_<help> <Esc>
map! <Help> <Esc>
map! i_<F1> <Esc>
map! <F1> <Esc>

" minibufexpl.vim
"map \mbe :MiniBufExplorer<cr>
"map \mbc :CMiniBufExplorer<cr>
"map \mbu :UMiniBufExplorer<cr>
"map \mbt :TMiniBufExplorer<cr>

"let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1

let g:Tex_CompileRule_dvi = 'latex-mk $*'
let g:Tex_CompileRule_pdf = 'latex-mk --pdflatex $*'
let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_ItemStyle_inparaenum = '\item'

let g:po_translator = 'Ryan Kavanagh <rak@debian.org>'

au BufEnter ~/.mutt/tmp/mutt-* set textwidth=72
