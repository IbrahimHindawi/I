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
badd +25 src/main.i
badd +46 src/main.c
badd +10038 term://C:/devel/I//21820:C:/WINDOWS/system32/cmd.exe
badd +79 src/main.i.c
badd +3406 term://C:/devel/I//47708:C:/WINDOWS/system32/cmd.exe
badd +1 C:/devel/I/i.bat
badd +1 C:/devel/I/extern/haikal/src/meta_arena/gen/saha.h
badd +4 LICENSE
badd +19 README.md
argglobal
%argdel
$argadd src/main.i
set stal=2
tabnew +setlocal\ bufhidden=wipe
tabrewind
edit src/main.i
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
exe 'vert 1resize ' . ((&columns * 40 + 181) / 363)
exe 'vert 2resize ' . ((&columns * 77 + 181) / 363)
exe 'vert 3resize ' . ((&columns * 60 + 181) / 363)
exe 'vert 4resize ' . ((&columns * 68 + 181) / 363)
exe 'vert 5resize ' . ((&columns * 114 + 181) / 363)
argglobal
enew
file neo-tree\ filesystem\ [1]
balt src/main.i
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
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
sil! 26,28fold
sil! 23,30fold
sil! 43,55fold
let &fdl = &fdl
23
sil! normal! zo
let s:l = 25 - ((24 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 25
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("src/main.i.c", ":p")) | buffer src/main.i.c | else | edit src/main.i.c | endif
if &buftype ==# 'terminal'
  silent file src/main.i.c
endif
balt src/main.i
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
sil! 14,16fold
sil! 19,22fold
sil! 35,36fold
sil! 42,44fold
sil! 39,46fold
sil! 49,50fold
sil! 53,65fold
sil! 68,69fold
sil! 72,73fold
sil! 76,77fold
let &fdl = &fdl
let s:l = 79 - ((56 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 79
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("term://C:/devel/I//47708:C:/WINDOWS/system32/cmd.exe", ":p")) | buffer term://C:/devel/I//47708:C:/WINDOWS/system32/cmd.exe | else | edit term://C:/devel/I//47708:C:/WINDOWS/system32/cmd.exe | endif
if &buftype ==# 'terminal'
  silent file term://C:/devel/I//47708:C:/WINDOWS/system32/cmd.exe
endif
balt src/main.i.c
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 3405 - ((41 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 3405
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("term://C:/devel/I//21820:C:/WINDOWS/system32/cmd.exe", ":p")) | buffer term://C:/devel/I//21820:C:/WINDOWS/system32/cmd.exe | else | edit term://C:/devel/I//21820:C:/WINDOWS/system32/cmd.exe | endif
if &buftype ==# 'terminal'
  silent file term://C:/devel/I//21820:C:/WINDOWS/system32/cmd.exe
endif
balt src/main.i
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 10042 - ((56 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 10042
normal! 0
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 40 + 181) / 363)
exe 'vert 2resize ' . ((&columns * 77 + 181) / 363)
exe 'vert 3resize ' . ((&columns * 60 + 181) / 363)
exe 'vert 4resize ' . ((&columns * 68 + 181) / 363)
exe 'vert 5resize ' . ((&columns * 114 + 181) / 363)
tabnext
edit src/main.c
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
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
exe '1resize ' . ((&lines * 46 + 30) / 60)
exe 'vert 1resize ' . ((&columns * 84 + 181) / 363)
exe '2resize ' . ((&lines * 46 + 30) / 60)
exe 'vert 2resize ' . ((&columns * 84 + 181) / 363)
exe '3resize ' . ((&lines * 46 + 30) / 60)
exe 'vert 3resize ' . ((&columns * 148 + 181) / 363)
argglobal
balt C:/devel/I/extern/haikal/src/meta_arena/gen/saha.h
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
sil! 1,43fold
sil! 58,82fold
sil! 86,90fold
sil! 104,107fold
sil! 110,114fold
sil! 117,123fold
sil! 126,139fold
sil! 142,146fold
sil! 149,155fold
sil! 158,162fold
sil! 165,169fold
sil! 172,178fold
sil! 181,190fold
sil! 193,196fold
sil! 199,202fold
sil! 205,208fold
sil! 211,212fold
sil! 215,216fold
sil! 219,220fold
sil! 223,229fold
sil! 242,245fold
sil! 247,251fold
sil! 254,255fold
sil! 253,257fold
sil! 263,265fold
sil! 260,273fold
sil! 278,280fold
sil! 275,284fold
sil! 287,291fold
sil! 295,313fold
sil! 316,320fold
sil! 240,324fold
sil! 232,327fold
sil! 331,332fold
sil! 330,334fold
sil! 338,339fold
sil! 337,341fold
sil! 345,346fold
sil! 344,348fold
sil! 352,354fold
sil! 351,356fold
sil! 360,363fold
sil! 359,365fold
sil! 368,369fold
sil! 372,373fold
sil! 376,377fold
sil! 380,384fold
sil! 390,394fold
sil! 402,404fold
sil! 408,411fold
sil! 400,417fold
sil! 389,422fold
sil! 425,429fold
sil! 439,445fold
sil! 454,456fold
sil! 463,465fold
sil! 462,467fold
sil! 452,476fold
sil! 482,484fold
sil! 481,486fold
sil! 479,493fold
sil! 448,500fold
sil! 503,506fold
sil! 438,512fold
sil! 517,526fold
sil! 515,528fold
sil! 532,538fold
sil! 531,540fold
sil! 545,556fold
sil! 543,558fold
sil! 563,574fold
sil! 561,576fold
sil! 579,580fold
sil! 583,587fold
sil! 591,598fold
sil! 609,610fold
sil! 603,613fold
sil! 615,622fold
sil! 601,631fold
sil! 590,637fold
sil! 648,652fold
sil! 657,666fold
sil! 640,670fold
sil! 686,688fold
sil! 682,690fold
sil! 695,703fold
sil! 694,705fold
sil! 714,716fold
sil! 673,720fold
sil! 733,736fold
sil! 739,742fold
sil! 750,751fold
sil! 729,754fold
sil! 723,757fold
sil! 761,762fold
sil! 760,764fold
sil! 767,769fold
sil! 772,774fold
sil! 777,780fold
sil! 782,784fold
sil! 781,786fold
sil! 794,797fold
sil! 799,801fold
sil! 803,806fold
sil! 808,811fold
sil! 814,815fold
sil! 817,818fold
sil! 813,820fold
sil! 791,821fold
sil! 836,839fold
sil! 834,840fold
sil! 830,842fold
sil! 828,844fold
sil! 856,859fold
sil! 854,860fold
sil! 864,867fold
sil! 862,868fold
sil! 851,870fold
sil! 849,873fold
sil! 876,877fold
sil! 875,880fold
sil! 882,884fold
sil! 847,886fold
sil! 824,887fold
sil! 903,906fold
sil! 901,907fold
sil! 898,909fold
sil! 896,911fold
sil! 921,924fold
sil! 919,925fold
sil! 916,927fold
sil! 914,929fold
sil! 939,942fold
sil! 937,943fold
sil! 934,945fold
sil! 932,947fold
sil! 950,952fold
sil! 955,956fold
sil! 890,957fold
sil! 960,963fold
sil! 966,967fold
sil! 972,973fold
sil! 976,977fold
sil! 982,984fold
sil! 981,987fold
sil! 989,992fold
sil! 994,997fold
sil! 980,998fold
sil! 1004,1005fold
sil! 1003,1008fold
sil! 1010,1014fold
sil! 1019,1023fold
sil! 1016,1025fold
sil! 1001,1027fold
sil! 1032,1033fold
sil! 1031,1034fold
sil! 1030,1036fold
sil! 1045,1046fold
sil! 1042,1047fold
sil! 1041,1048fold
sil! 1051,1052fold
sil! 1055,1056fold
sil! 1054,1057fold
sil! 1039,1058fold
sil! 1066,1068fold
sil! 1069,1070fold
sil! 1071,1072fold
sil! 1064,1073fold
sil! 1079,1080fold
sil! 1082,1083fold
sil! 1078,1084fold
sil! 1085,1086fold
sil! 1076,1087fold
sil! 1091,1093fold
sil! 1098,1100fold
sil! 1103,1105fold
sil! 1096,1106fold
sil! 1090,1107fold
sil! 1112,1114fold
sil! 1118,1120fold
sil! 1117,1121fold
sil! 1122,1123fold
sil! 1110,1126fold
sil! 1133,1134fold
sil! 1130,1135fold
sil! 1143,1144fold
sil! 1140,1145fold
sil! 1138,1146fold
sil! 1129,1147fold
sil! 1154,1156fold
sil! 1158,1160fold
sil! 1162,1165fold
sil! 1167,1172fold
sil! 1176,1177fold
sil! 1178,1179fold
sil! 1180,1181fold
sil! 1182,1183fold
sil! 1184,1185fold
sil! 1174,1188fold
sil! 1191,1196fold
sil! 1197,1201fold
sil! 1202,1203fold
sil! 1207,1209fold
sil! 1190,1212fold
sil! 1152,1213fold
sil! 1222,1224fold
sil! 1218,1227fold
sil! 1229,1233fold
sil! 1235,1240fold
sil! 1242,1245fold
sil! 1216,1246fold
sil! 1253,1259fold
sil! 1249,1261fold
sil! 1275,1281fold
sil! 1264,1283fold
sil! 1291,1296fold
sil! 1299,1301fold
sil! 1286,1303fold
sil! 1311,1316fold
sil! 1306,1318fold
sil! 1333,1338fold
sil! 1341,1343fold
sil! 1321,1345fold
sil! 1360,1365fold
sil! 1348,1367fold
sil! 1375,1376fold
sil! 1373,1377fold
sil! 1387,1391fold
sil! 1380,1392fold
sil! 1395,1397fold
sil! 1404,1405fold
sil! 1402,1406fold
sil! 1416,1420fold
sil! 1410,1421fold
sil! 1429,1430fold
sil! 1427,1431fold
sil! 1441,1445fold
sil! 1435,1446fold
sil! 1370,1447fold
sil! 1450,1455fold
sil! 1465,1467fold
sil! 1484,1486fold
sil! 1458,1490fold
let &fdl = &fdl
let s:l = 46 - ((22 * winheight(0) + 23) / 46)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 46
normal! 011|
wincmd w
argglobal
enew
balt README.md
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
if bufexists(fnamemodify("README.md", ":p")) | buffer README.md | else | edit README.md | endif
if &buftype ==# 'terminal'
  silent file README.md
endif
balt src/main.c
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=99
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 19 - ((18 * winheight(0) + 23) / 46)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 19
normal! 03|
wincmd w
exe '1resize ' . ((&lines * 46 + 30) / 60)
exe 'vert 1resize ' . ((&columns * 84 + 181) / 363)
exe '2resize ' . ((&lines * 46 + 30) / 60)
exe 'vert 2resize ' . ((&columns * 84 + 181) / 363)
exe '3resize ' . ((&lines * 46 + 30) / 60)
exe 'vert 3resize ' . ((&columns * 148 + 181) / 363)
tabnext 1
set stal=1
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
