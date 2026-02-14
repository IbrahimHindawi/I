let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd C:/devel/I
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +1 C:/devel/I/CMakeLists.txt
badd +13 C:/devel/I/README.md
badd +12 C:/devel/I/src/main.i
badd +52 term://C:/devel/I//9440:C:/WINDOWS/system32/cmd.exe
badd +178 C:/devel/I/src/example_main.c
badd +366 C:/devel/I/src/main.cpp
badd +102 C:/devel/I/src/main.c
badd +133 term://C:/devel/I//53856:C:/WINDOWS/system32/cmd.exe
badd +33 C:/devel/I/src/main.i.c
argglobal
%argdel
edit C:/devel/I/src/main.i.c
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
4wincmd h
wincmd w
wincmd w
wincmd w
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 40 + 159) / 318)
exe 'vert 2resize ' . ((&columns * 69 + 159) / 318)
exe 'vert 3resize ' . ((&columns * 68 + 159) / 318)
exe 'vert 4resize ' . ((&columns * 69 + 159) / 318)
exe 'vert 5resize ' . ((&columns * 68 + 159) / 318)
argglobal
enew
file neo-tree\ filesystem\ [1]
balt C:/devel/I/src/main.i
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
wincmd w
argglobal
balt C:/devel/I/src/main.i
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
sil! 3,5fold
sil! 8,11fold
sil! 19,21fold
sil! 16,23fold
sil! 26,27fold
sil! 35,36fold
sil! 39,43fold
sil! 46,47fold
sil! 50,51fold
let &fdl = &fdl
let s:l = 53 - ((46 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 53
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("C:/devel/I/src/main.i", ":p")) | buffer C:/devel/I/src/main.i | else | edit C:/devel/I/src/main.i | endif
if &buftype ==# 'terminal'
  silent file C:/devel/I/src/main.i
endif
balt C:/devel/I/src/main.c
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
sil! 20,24fold
let &fdl = &fdl
let s:l = 12 - ((11 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 12
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("term://C:/devel/I//53856:C:/WINDOWS/system32/cmd.exe", ":p")) | buffer term://C:/devel/I//53856:C:/WINDOWS/system32/cmd.exe | else | edit term://C:/devel/I//53856:C:/WINDOWS/system32/cmd.exe | endif
if &buftype ==# 'terminal'
  silent file term://C:/devel/I//53856:C:/WINDOWS/system32/cmd.exe
endif
balt C:/devel/I/src/main.c
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 111 - ((2 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 111
normal! 011|
wincmd w
argglobal
if bufexists(fnamemodify("term://C:/devel/I//9440:C:/WINDOWS/system32/cmd.exe", ":p")) | buffer term://C:/devel/I//9440:C:/WINDOWS/system32/cmd.exe | else | edit term://C:/devel/I//9440:C:/WINDOWS/system32/cmd.exe | endif
if &buftype ==# 'terminal'
  silent file term://C:/devel/I//9440:C:/WINDOWS/system32/cmd.exe
endif
balt C:/devel/I/src/main.i
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 64 - ((42 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 64
normal! 02|
wincmd w
3wincmd w
exe 'vert 1resize ' . ((&columns * 40 + 159) / 318)
exe 'vert 2resize ' . ((&columns * 69 + 159) / 318)
exe 'vert 3resize ' . ((&columns * 68 + 159) / 318)
exe 'vert 4resize ' . ((&columns * 69 + 159) / 318)
exe 'vert 5resize ' . ((&columns * 68 + 159) / 318)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
