" Vim indent file
" Language:	R
" Author:	Jeremy Stephens <jeremy.f.stephens@vanderbilt.edu>
" URL:		
" Last Change:	
" Version:	
" Notes:  
" Changes: 
" Options: 

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

setlocal indentexpr=GetRIndent()
"setlocal indentkeys+=0=,0),=EO
setlocal indentkeys+=0=,0),=EO,=>

" Only define the function once.
if exists("*GetRIndent")
	finish
endif

function GetRIndent()
	" Find a non-blank line above the current line.
	let lnum = prevnonblank(v:lnum - 1)
	" Hit the start of the file, use zero indent.
	if lnum == 0
		return 0
	endif
    
	let line = getline(lnum)      " last line
	let cline = getline(v:lnum)   " current line
	let pline = getline(lnum - 1) " previous to last line
	let ind = indent(lnum)

	" Indent blocks enclosed by {} or ()
	"if line =~ '[{(]\s*\(#[^)}]*\)\=$'
	if line =~ '[{(]\s*[^)}]*$'
		let ind = ind + &sw
	endif
	if cline =~ '^\s*[)}]'
		let ind = ind - &sw
	endif
    
	return ind
endfunction
" vim: set ts=4 sw=4:
