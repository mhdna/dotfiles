vim.cmd [[
  augroup _general_settings
    autocmd!
    autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    " Do not keep netrw buffers open in the background
    autocmd FileType netrw setl bufhidden=delete
    autocmd FileType qf set nobuflisted

  " Return to last edit position when opening files (You want this!)
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif

  " remove trailing whitespace
  autocmd BufWritePre * let currPos = getpos(".")
  autocmd BufWritePre * %s/\s\+$//e
  autocmd BufWritePre * %s/\n\+\%$//e
  autocmd BufWritePre *.[ch] %s/\%$/\r/e
  autocmd BufWritePre * cal cursor(currPos[1], currPos[2])
]]

-- highlight on yank
vim.api.nvim_create_autocmd({ "TextYankPost" }, {
    pattern = { "*"},
    command = "lua require('vim.highlight').on_yank({higroup = 'Visual', timeout = 200})",
})
-- disable automatic commenting new lines
vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
    pattern = { "*"},
    command = "set formatoptions-=cro",
})

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
    pattern = { "Xresources", "Xdefaults", "xresources", "xdefaults" },
    command = "!xrdb %",
})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "Xresources", "Xdefaults", "xresources", "xdefaults" },
    command = "set filetype=xdefaults",
})

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
    pattern = { "bm-dirs", "bm-files"},
    command = "!shortcuts",
})


vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "*.me", "*.mom", "*.man" },
    command = "set filetype=groff",
})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "*.tex" },
    command = "set filetype=tex",
})


-- vim.api.nvim_create_autocmd({"BufWritePre"}, {
--     pattern = {"*"},
--     command = "let currPos = getpos("."")",
-- })
--

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
    pattern = { "*.tex" },
    command = 'silent! execute "!compiler % >/dev/null 2>&1" | redraw!',
})

vim.api.nvim_create_autocmd({ "VimLeave" }, {
    pattern = { "*.tex" },
    command = '!texclear %',
})


-- auto formatting
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    pattern = { "*.java", "*.cpp", "*.py" },
    command = "lua vim.lsp.buf.formatting_sync()",
})

-- alpha
--   augroup _alpha
--     autocmd!
-- autocmd User AlphaReady set showtabline=0 | autocmd BufUnload <buffer> set showtabline=2
-- augroup end
