if !filereadable(expand("%:p:h")."/Makefile")
    setlocal makeprg=gcc\ -Wall\ -Wextra\ -o\ %<\ %
endif

nnoremap <leader>r :call RunFile()<cr>
nnoremap <leader>t :make \| call RunFile()<cr>

function RunFile()
    !./%:r
endfunction
