" Vim syntax file for I language
" Maintainer: Codex

if exists("b:current_syntax")
  finish
endif

syn case match

" Preprocessor passthrough and comments
syn match iPreProc "^\s*#\s*\(define\|include\|if\|ifdef\|ifndef\|elif\|else\|endif\|undef\|pragma\|error\|warning\|line\)\>.*$" contains=iString,iNumber
syn match iComment "^\s*#.*$" contains=iTodo
syn keyword iTodo TODO FIXME NOTE contained

" Keywords
syn keyword iKeyword proc struct enum union alias const return import external define
syn keyword iConditional if else switch case default
syn keyword iRepeat for while do break continue
syn keyword iOperatorWord and or shl shr

" Built-in types / primitives
syn keyword iType i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 usize b32 bool void char

" Common C / Win32 interop types
syn keyword iInteropType HWND HINSTANCE HCURSOR HMENU HBRUSH HDC ATOM BOOL DWORD UINT WPARAM LPARAM LRESULT MSG WNDCLASSA PAINTSTRUCT RECT

" Built-ins
syn keyword iBuiltin cast sizeof alignof printf null

" Declarations
syn match iDeclName "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:\s*\(proc\|struct\|enum\|union\|alias\)"
syn match iFieldName "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:"
syn match iCallConv "\<proc\s*\[\s*[A-Za-z_][A-Za-z0-9_]*\s*\]" contains=iKeyword,iCallConvName
syn match iCallConvName "\[\s*\zs[A-Za-z_][A-Za-z0-9_]*\ze\s*\]" contained

" Generic sugar: array<T>reserve / make<T>(...)
syn match iGenericName "\<[A-Za-z_][A-Za-z0-9_]*\s*<[^>]\+>[A-Za-z_][A-Za-z0-9_]*\>"
syn match iGenericCall "\<[A-Za-z_][A-Za-z0-9_]*\s*<[^>]\+>\ze\s*("

" Numbers
syn match iNumber "\<0x[0-9A-Fa-f]\+\>"
syn match iNumber "\<0b[01]\+\>"
syn match iNumber "\<[0-9]\+\>"
syn match iNumber "\<[0-9]\+\.[0-9]\+\>"
syn match iNumber "\<[0-9]\+\(u64\|i64\|u32\|i32\|f32\|f64\)\>"

" Strings
syn region iString start=+"+ skip=+\\\\\|\\"+ end=+"+

" Operators and punctuation
syn match iOperator "->"
syn match iOperator "==\|!=\|<=\|>="
syn match iOperator "+=\|-=\|\*=\|/=\|%=\|&=\|\^=\||="
syn match iOperator "[:=,;.@&|^%*/!+-]"
syn match iDelimiter "[(){}\[\]<>]"

" Identifier fallback last
syn match iIdentifier "\<[A-Za-z_][A-Za-z0-9_]*\>"

" Highlight links
hi def link iPreProc PreProc
hi def link iComment Comment
hi def link iTodo Todo
hi def link iKeyword Keyword
hi def link iConditional Conditional
hi def link iRepeat Repeat
hi def link iOperatorWord Operator
hi def link iType Type
hi def link iInteropType Type
hi def link iBuiltin Function
hi def link iDeclName Function
hi def link iFieldName Identifier
hi def link iCallConv Special
hi def link iCallConvName Special
hi def link iGenericName Function
hi def link iGenericCall Function
hi def link iIdentifier Identifier
hi def link iNumber Number
hi def link iString String
hi def link iOperator Operator
hi def link iDelimiter Delimiter

let b:current_syntax = "i"
