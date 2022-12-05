vim.cmd [[
  augroup _general_settings
    autocmd!
    autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    autocmd TextYankPost * silent!lua require('vim.highlight').on_yank({higroup = 'Visual', timeout = 200})
    autocmd BufWinEnter * :set formatoptions-=cro
    autocmd FileType qf set nobuflisted
  augroup end

  " augroup _git
  "   autocmd!
  "   autocmd FileType gitcommit setlocal wrap
  "   autocmd FileType gitcommit setlocal spell
  " augroup end

  " augroup _markdown
  "   autocmd!
  "   autocmd FileType markdown setlocal wrap
  "   autocmd FileType markdown setlocal spell
  " augroup end

  augroup _auto_resize
    autocmd!
    autocmd VimResized * tabdo wincmd =
  augroup end

" augroup _lsp
"   autocmd!
"   autocmd BufWritePre * lua vim.lsp.buf.format()
" augroup end

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif


    " Cpp compile
	autocmd filetype cpp nnoremap <leader>c :w! \| :split \| term g++ % -o %< && ./%< <CR>i
    " java compile
	autocmd filetype java nnoremap <leader>c :w! \| :split \| term java % <CR>
	" autocmd filetype java nnoremap <Leader>C :w! \| :split \| :term cd $pwd ; !javac *.java ; java Tester <CR>
    " tex autocompile
	" autocmd BufWritePost *.tex silent! execute "!compiler % >/dev/null 2>&1" | redraw!
	autocmd VimLeave *.tex !texclear %

  autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
  autocmd BufRead,BufNewFile *.tex set filetype=tex
  autocmd BufRead,BufNewFile *.cpp set filetype=cpp
  " autocmd BufRead,BufNewFile *.md,*.markdown,*.mmd setlocal filetype=markdown
  " autocmd BufRead,BufNewFile *.md,*.markdown,*.mmd UltiSnipsAddFiletypes ghmarkdown.markdown
  " Automatically deletes all trailing whitespace and newlines at end of file on save. & reset cursor position
  autocmd BufWritePre * let currPos = getpos(".")
  autocmd BufWritePre * %s/\s\+$//e
  autocmd BufWritePre * %s/\n\+\%$//e
  autocmd BufWritePre *.[ch] %s/\%$/\r/e
  autocmd BufWritePre * cal cursor(currPos[1], currPos[2])
  " When shortcut files are updated, renew bash and ranger configs with new material:
  autocmd BufWritePost bm-files,bm-dirs !shortcuts
  " Run xrdb whenever Xdefaults or Xresources are updated.
  autocmd BufRead,BufNewFile Xresources,Xdefaults,xresources,xdefaults set filetype=xdefaults
  autocmd BufWritePost Xresources,Xdefaults,xresources,xdefaults !xrdb %
  " markdown pandoc syntax highlighting
  " augroup pandoc_syntax
  " autocmd! FileType *.md set syntax=markdown.pandoc
  " augroup END
  " autocmd BufRead,BufNewFile *.md set filetype=text
]]

-- Autoformat
-- augroup _lsp
-- autocmd!
-- autocmd BufWritePre * lua vim.lsp.buf.formatting()
-- augroup end
-- alpha
--   augroup _alpha
--     autocmd!
-- autocmd User AlphaReady set showtabline=0 | autocmd BufUnload <buffer> set showtabline=2
-- augroup end
