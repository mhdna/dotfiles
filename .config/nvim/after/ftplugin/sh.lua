vim.opt_local.makeprg = "shellcheck -f gcc '%'"

local opts = { noremap = true, silent = true, buffer = 0 }

vim.keymap.set('n', '<F9>', ':w<CR>:silent make<CR>:cwindow<CR><C-w><C-k>', opts)
vim.keymap.set('n', '<leader>q', ':cwindow<CR>', opts)
vim.keymap.set('n', '<leader>s', ':source ~/.config/nvim/ftplugin/sh.lua<CR>', opts)
vim.keymap.set('n', ']d', ':cnext<CR>', opts)
vim.keymap.set('n', '[d', ':cprevious<CR>', opts)
