" Vim syntax file
" Language:     bel
" Filenames:    *.bel
" Maintainers:  Vincent Aravantinos <vincent.aravantinos@gmail.com>
" Last Change:  2010 Jun 9 - Initial version.
" TODO see what the Twelf vim file does

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax") && b:current_syntax == "beluga"
  finish
endif

" bel is not case sensitive.
"syn case match

"syn match belError "\S" 
syn match  belComment   containedin=ALL  "%.*" extend

syn match   belId       contained "[[:lower:]][[:lower:][:digit:]']*"
syn keyword belTermOp   contained type
syn match   belTermOp   contained "\(->\|<-\|\\\|\.\|_\|:\)"
syn region  belTerm     contained contains=@belTermClu matchgroup=belTermOp start="(" end=")"
syn region  belTerm     contained contains=belId,belColonType,@belTermClu matchgroup=belTermOp start="{" end="}"
syn match   belColType  contained ":"
syn cluster belTermClu  contains=belTerm,belTermOp
syn region  belLf_decl  contains=belColType,@belTermClu start="[[:lower:]&][[:lower:][:digit:]']*\_s*:" matchgroup=belDot end="\."
syn region  belLf_decl  matchgroup=belStmt start="%name\>" matchgroup=belDot end="\."
syn match   belEq       contained "="
syn match   belColDecl  contained ":"
syn region  belDecl     contained contains=@belCTermClu,belColDecl matchgroup=belBlockBraces start="{" end="}"
syn region  belBlock    contained contains=belDecl matchgroup=belStmt start="\<block\>" end=";"
syn region  belSomeCtnt contained contains=belS_decl matchgroup=belSomeBrackets start="\[" end="]"
syn region  belCTermPar contained contains=@belCTermClu matchgroup=belCTerm start="(" end=")"
syn match   belCTerm    contained "\.\s*[[:digit:]]\+"
syn match   belCTerm    contained "\(#\|\.\.\|\<_\>\)"
syn keyword belCTerm    contained sigma
syn match   belS_decl   contained "\(:\|,\)"
syn region  belCTermBck contained contains=belS_decl matchgroup=belCTerm start="\<block\>" end="\."
syn cluster belCTermClu contains=belCTerm,belCTermBck,belCTermPar
syn region  belSome     contained contains=belBlock,belSomeCtnt matchgroup=belC_typOp start="\<some\>" end=";"
syn region  belSchDef   contained contains=belSome,@belCTermClu matchgroup=belEq start="=" end=";"
syn region  belC_decl   contains=belSchDef matchgroup=belStmt start="\<schema\>" matchgroup=belDot end=";" keepend
syn region  belExp      contained contains=@belExpClu matchgroup=belExpOp start="(" end=")"
syn match   belCtx      contained "\(:\|,\|\<block\>\)"
syn match   belCtxExp   contained "\(:\|,\|\<block\>\|\.\)"
syn region  belExp      contained contains=belCtxExp,@belCTermClu matchgroup=belExpOp start="\[" end="]"
syn keyword belExpOp    contained FN fn mlam let in case of
syn match   belBindDot  contained "\."
syn region  belDepApp   contained contains=belCtxExp,belBindDot,@belCTermClu matchgroup=belExpOp start="<" end=">"
syn match   belExpOp    contained "\(=>\||\|:\|=\|\\\|\.\|<<\)"
syn region  belLetDef   contained contains=@belExpClu matchgroup=belEq start="=" end=";"
syn cluster belExpClu   contains=belExp,@belC_typExp,belExpOp,@belCTermClu,belDepApp
syn match   belC_typOp  contained "->"
syn match   belCols     contained ":"
"syn region  belCtypCtx  contained contains=belCtx,@belCTermClu matchgroup=belC_typOp start="\[" end="]"
syn region  belCtypCtx  contained contains=belCtx matchgroup=belC_typOp start="\[" end="]"
syn region  belC_typBr  contained contains=belCols,@belCTermClu,belCtypCtx,belKleeneGr matchgroup=belC_typOp start="{" end="}"
syn region  belKleeneGr contained contains=@belC_typ matchgroup=belKleene start="(" end=")\*\?"
"syn cluster belC_typ    contains=belC_typOp,belC_typBr,belSome,belKleeneGr,belCtypCtx,@belCTermClu
syn cluster belC_typ    contains=belC_typOp,belC_typBr,belSome,belKleeneGr,belCtypCtx

syn region  belCtypCtxExp contained contains=belCtxExp,@belCTermClu matchgroup=belC_typOpExp start="\[" end="]"
syn match   belColsExp    contained ":"
syn region  belKleeneExpGr contained contains=@belC_typExp matchgroup=belKleeneExp start="(" end=")\*\?"
syn region  belCtypCtxExp contained contains=belCtxExp,@belCTermClu matchgroup=belC_typOpExp start="\[" end="]"
syn region  belC_typBrExp contained contains=belColsExp,@belCTermClu,belCtypCtxExp matchgroup=belC_typOpExp start="{" end="}"
syn match   belC_typOpExp contained "->"
syn cluster belC_typExp   contains=@belCTermClu,belC_typOpExp,belC_typBrExp,belSome,belCtypCtxExp

syn region  belLetSpec  contained contains=belLetDef,@belC_typ matchgroup=belEq start=":" end=";"
syn region  belC_decl   contains=belLetDef,belLetSpec matchgroup=belStmt start="\<let\>" matchgroup=belDot end=";" keepend
syn region  belRecDef   contained contains=@belExpClu matchgroup=belEq start="=" end="\(;\|and\)"
syn region  belRecSpec  contained contains=belRecDef,@belC_typ matchgroup=belEq start=":" end="\(;\|and\)"
syn region  belC_decl   contains=belRecDef,belRecSpec matchgroup=belStmt start="\<rec\>" matchgroup=belDot end=";" keepend


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_bel_syntax_inits")
  if version < 508
    let did_beluga_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink belError		     Error
  HiLink belComment      Comment
  HiLink belKwd          PreProc
  HiLink belCtxt         Type
  HiLink belTypeKwd      Keyword
  HiLink belOperators    Constant
  HiLink belTermOp       Type
  HiLink belStmt         PreProc
  HiLink belDot          belStmt
  HiLink belEq           belStmt
  HiLink belColType      belStmt
  HiLink belSomeBrackets belC_typOp
  HiLink belCTerm        belExpOp
  HiLink belS_decl       belC_typOp
  HiLink belBlockBraces  belSomeBrackets
  HiLink belColDecl      belBlockBraces
  HiLink belExpOp        Keyword
  HiLink belBindDot      belExpOp
  HiLink belC_typOp      Type
  HiLink belC_typOpExp   belExpOp
  HiLink belKleene       belC_typOp
  HiLink belKleeneExp    belExpOp
  HiLink belCols         belC_typOp
  HiLink belColsExp      belExpOp
  HiLink belCtx          belC_typOp
  HiLink belCtxExp       belExpOp

  delcommand HiLink
endif

let b:current_syntax = "belgua"
