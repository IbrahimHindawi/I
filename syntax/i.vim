" Vim syntax file for I language
" Maintainer: Codex

if exists("b:current_syntax")
  finish
endif

syn case match
syn sync minlines=80

" Preprocessor passthrough and comments
syn match iPreProc "^\s*#\s*\(define\|include\|if\|ifdef\|ifndef\|elif\|else\|endif\|undef\|pragma\|error\|warning\|line\)\>.*$" contains=iString,iChar,iNumber,iTodo
syn match iComment "^\s*#.*$" contains=iTodo
syn keyword iTodo TODO FIXME NOTE contained

" Keywords
syn keyword iKeyword proc return import define
syn keyword iStructure struct enum union alias
syn keyword iStorageClass const
syn keyword iExternal external external_emit
syn keyword iConditional if else switch case default
syn keyword iRepeat for while do break continue
syn keyword iOperatorWord and or shl shr
syn keyword iBoolean true false

" Built-in types / primitives
syn keyword iCoreType i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 usize b32 bool void char
syn keyword iCoreType va_list FILE

" Common C / Win32 interop types
syn keyword iInteropType HWND HINSTANCE HMODULE HCURSOR HMENU HBRUSH HDC HGDIOBJ ATOM BOOL DWORD UINT INT LONG ULONG HRESULT WPARAM LPARAM LRESULT MSG WNDCLASSA PAINTSTRUCT RECT
syn keyword iInteropType ID3D11Device ID3D11DeviceContext IDXGISwapChain ID3D11Buffer ID3D11Texture2D ID3D11RenderTargetView ID3D11DepthStencilView ID3D11ShaderResourceView ID3D11SamplerState ID3DBlob

" Built-ins
syn keyword iBuiltin alignof cast null offsetof printf sizeof va_arg va_end va_start

" Identifier fallback before generic/declaration-specific matches, so they can win.
syn match iIdentifier "\<[A-Za-z_][A-Za-z0-9_]*\>"

" Declarations
syn match iFieldName "\(^\|[^>]\)\zs\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:"
syn match iProcDeclName "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:\s*proc"
syn match iProcDeclName "\<[A-Za-z_][A-Za-z0-9_]*\s*<[^>]\+>[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:\s*proc"
syn match iDeclName "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:\s*\(struct\|enum\|union\|alias\)"
syn match iDeclName "\<[A-Za-z_][A-Za-z0-9_]*\s*<[^>]\+>[A-Za-z_][A-Za-z0-9_]*\>\ze\s*:\s*\(struct\|enum\|union\|alias\)"
syn match iCallConv "\<proc\s*\[\s*[A-Za-z_][A-Za-z0-9_]*\s*\]" contains=iKeyword,iCallConvName
syn match iCallConvName "\[\s*\zs[A-Za-z_][A-Za-z0-9_]*\ze\s*\]" contained

" Generic container/type spelling. Keep punctuation as normal delimiters:
" Array<i32>reserve => Array/i32 are type colored, reserve is proc colored.
syn match iGenericTypeToken "\<[A-Za-z_][A-Za-z0-9_]*\s*<\s*[A-Za-z_][A-Za-z0-9_]*\s*>" contains=iGenericArg,iGenericCoreArg,iGenericHead,iGenericDelimiter transparent
syn match iGenericProcToken "\<[A-Za-z_][A-Za-z0-9_]*\s*<\s*[A-Za-z_][A-Za-z0-9_]*\s*>\s*[A-Za-z_][A-Za-z0-9_]*\>" contains=iGenericArg,iGenericCoreArg,iGenericHead,iGenericProcTail,iGenericDelimiter transparent
syn match iGenericArg "\<[A-Za-z_][A-Za-z0-9_]*\>" contained
syn keyword iGenericCoreArg i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 usize b32 bool void char contained
syn match iGenericHead "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*<" contained
syn match iGenericProcTail "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*[:(]" contained
syn match iGenericDelimiter "[<>]" contained

" Calls. Do not color the proc keyword in declarations like name:proc(...).
syn match iProcCallName "\(^\|[^:]\)\zs\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*("

" Numbers
syn match iNumber "\<0x[0-9A-Fa-f]\+\>"
syn match iNumber "\<0b[01]\+\>"
syn match iNumber "\<[0-9]\+\([uif][0-9]\+\)\?\>"
syn match iNumber "\<[0-9]\+\.[0-9]*\(f32\|f64\|f\)\?\>"
syn match iNumber "\<[0-9]*\.[0-9]\+\(f32\|f64\|f\)\?\>"

" Strings
syn region iString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=iEscape
syn region iChar start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=iEscape
syn match iEscape "\\\(.\|x[0-9A-Fa-f]\{2}\)" contained

" Operators and punctuation
syn match iOperator "\.\*"
syn match iOperator "\.&"
syn match iOperator "->"
syn match iOperator "\.\.\."
syn match iOperator "==\|!=\|<=\|>="
syn match iOperator "&&\|||"
syn match iOperator "+=\|-=\|\*=\|/=\|%=\|&=\|\^=\||="
syn match iOperator "[:=,;.@&|^%*/!+?~-]"
syn match iDelimiter "[(){}\[\]<>]"
syn keyword iCoreTypeInType i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 usize b32 bool void char va_list FILE contained
syn match iTypeName "\%(\<proc\>\|\<struct\>\|\<enum\>\|\<union\>\|\<alias\>\|\<external\>\|\<external_emit\>\)\@!\<[A-Za-z_][A-Za-z0-9_]*\>\%(\s*<\)\@!" contained
syn match iTypePrefix ":\|->" nextgroup=iTypePointerPrefix,iFixedArrayPrefix,iCoreTypeInType,iTypeName skipwhite
syn match iTypePointerPrefix "\*" contained nextgroup=iTypePointerPrefix,iFixedArrayPrefix,iCoreTypeInType,iTypeName skipwhite
syn match iFixedArrayPrefix "\[[0-9]*]" contained nextgroup=iTypePointerPrefix,iCoreTypeInType,iTypeName skipwhite

" Highlight links
hi def link iPreProc PreProc
hi def link iComment Comment
hi def link iTodo Todo
hi def link iKeyword Keyword
hi def link iStructure Type
hi def link iStorageClass StorageClass
hi def link iExternal StorageClass
hi def link iConditional Conditional
hi def link iRepeat Repeat
hi def link iOperatorWord Operator
hi def link iBoolean Boolean
hi def link iCoreType Type
hi def link iCoreTypeInType Type
hi def link iInteropType Structure
hi def link iBuiltin Function
hi def link iProcDeclName Function
hi def link iProcCallName Function
hi def link iDeclName Structure
hi def link iTypeName Structure
hi def link iTypePrefix Operator
hi def link iTypePointerPrefix Operator
hi def link iFixedArrayPrefix Delimiter
hi def link iFieldName Identifier
hi def link iCallConv Special
hi def link iCallConvName Special
hi def link iGenericHead Structure
hi def link iGenericArg Structure
hi def link iGenericCoreArg Type
hi def link iGenericProcTail Function
hi def link iGenericDelimiter Delimiter
hi def link iIdentifier Identifier
hi def link iNumber Number
hi def link iString String
hi def link iChar Character
hi def link iEscape SpecialChar
hi def link iOperator Operator
hi def link iDelimiter Delimiter

let b:current_syntax = "i"
