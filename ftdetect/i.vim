augroup i_filetype
  autocmd!
  autocmd BufRead,BufNewFile *.i setlocal filetype=i syntax=i
  autocmd BufRead,BufNewFile *.I setlocal filetype=i syntax=i
augroup END
