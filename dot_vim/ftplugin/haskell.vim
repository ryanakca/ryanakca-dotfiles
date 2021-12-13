"
" general Haskell source settings
" (shared functions are in autoload/haskellmode.vim)
"
" (Claus Reinke, last modified: 21/04/2009)
"
" part of haskell plugins: http://projects.haskell.org/haskellmode-vim
" please send patches to <claus.reinke@talk21.com>

" try gf on import line, or ctrl-x ctrl-i, or [I, [i, ..
set include=^import\\s*\\(qualified\\)\\?\\s*
set includeexpr=substitute(v:fname,'\\.','/','g').'.hs'

