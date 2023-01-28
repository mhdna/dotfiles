local bufopts = { noremap = true, silent = true, buffer = bufnr }

vim.cmd [[
" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
\ if line("'\"") > 0 && line("'\"") <= line("$") |
\   exe "normal! g`\"" |
\ endif
]]

-- highlight on yank
vim.api.nvim_create_autocmd({ "TextYankPost" }, {
    pattern = { "*" },
    callback = function()
        vim.highlight.on_yank({
            higroup = 'IncSearch',
            timeout = 200,
        })
    end,
})

-- disable automatic commenting new lines
vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
    pattern = { "*" },
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
    pattern = { "bm-dirs", "bm-files" },
    command = "!shortcuts",
})


vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "*.me", "*.mom", "*.man" },
    command = "set filetype=groff",
})

-- tex settings
vim.api.nvim_create_autocmd("FileType", {
    pattern = { "tex" },
    callback = function ()
        vim.api.nvim_create_autocmd({'BufWritePre', 'BufNewFile'}, {
                command = "set filetype=tex",
        })
        vim.api.nvim_create_autocmd('BufWritePost', {
            command = 'silent! execute "!compiler % >/dev/null 2>&1" | redraw!',
        })
        vim.api.nvim_create_autocmd('VimLeave', {
            command = '!texclear %',
        })
    end,
})

-- Remove trailing whitespace
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    pattern = "*",
    command = "%s/\\s\\+$//e",
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = {'netrw', 'qf', 'help', 'man', 'lspinfo'},
    callback = function()
        vim.keymap.set('n', 'q', ":close<CR>", bufopts)
   end,
})

-- Do not keep netrw buffers open in the background
vim.api.nvim_create_autocmd('FileType', {
    pattern = 'netrw' ,
    callback = function()
       vim.bo.bufhidden=delete
   end,
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = 'qf' ,
    callback = function()
       vim.bo.buflisted= true
   end,
})

-- autoformat
vim.api.nvim_create_autocmd('FileType', {
    pattern = {'python', 'java', 'cpp'},
    callback = function ()
        vim.api.nvim_create_autocmd('BufWritePre', {
            callback = function()
                vim.lsp.buf.format()
            end
        })
    end,
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = 'cpp',
    callback = function ()
                vim.keymap.set("n", "<leader>c", ":w! | :split | term g++ % -o %< && ./%< <CR>i", bufopts)
    end,
})



-- alpha
--   augroup _alpha
--     autocmd!
-- autocmd User AlphaReady set showtabline=0 | autocmd BufUnload <buffer> set showtabline=2
-- augroup end

-- vim.api.nvim_create_autocmd({"BufWritePre"}, {
--     pattern = {"*"},
--     command = "let currPos = getpos("."")",
-- })
--

