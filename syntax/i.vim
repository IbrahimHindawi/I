" Vim syntax file for I language
" Maintainer: Codex

if exists("b:current_syntax")
  finish
endif

syn case match

" Comments
syn match iComment "#.*$"

" Keywords
syn keyword iKeyword proc struct return for if else import external define

" Built-in types / primitives
syn keyword iType i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 usize b32 bool void char

" Built-ins
syn keyword iBuiltin cast sizeof alignof printf null

" Qualified generic sugar: array<T>reserve
syn match iIdentifier "\<[A-Za-z_][A-Za-z0-9_]*\s*<[^>]\+>[A-Za-z_][A-Za-z0-9_]*\>"

" Identifiers (fallback)
syn match iIdentifier "\<[A-Za-z_][A-Za-z0-9_]*\>"

" Numbers
syn match iNumber "\<[0-9]\+\>"
syn match iNumber "\<[0-9]\+\.[0-9]\+\>"
syn match iNumber "\<[0-9]\+u64\>"

" Strings
syn region iString start=+"+ skip=+\\\\\|\\"+ end=+"+

" Operators and punctuation
syn match iOperator "->"
syn match iOperator "==\|!="
syn match iOperator "+=\|-=\|\*=\|/=\|&=\|\^=\||="
syn match iOperator "[:=,;.@&|^*/!+-]"
syn match iDelimiter "[(){}\[\]<>]"

" Highlight links
hi def link iComment Comment
hi def link iKeyword Keyword
hi def link iType Type
hi def link iBuiltin Function
hi def link iIdentifier Identifier
hi def link iNumber Number
hi def link iString String
hi def link iOperator Operator
hi def link iDelimiter Delimiter

let b:current_syntax = "i"
