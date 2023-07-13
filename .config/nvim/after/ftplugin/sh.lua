vim.opt_local.makeprg = "shellcheck -f gcc '%'"

local opts = { noremap = true, silent = true, buffer = 0 }

-- TODO: open cwindow or focus
vim.keymap.set('n', '<leader>c', ':w<CR>:silent make<CR>:cwindow<CR><C-w><C-k>', opts)
vim.keymap.set('n', '<leader>q', ':cwindow<CR>', opts)
vim.keymap.set('n', '<leader>s', ':source ~/.config/nvim/ftplugin/sh.lua<CR>', opts)
vim.keymap.set('n', '<M-n>', ':cnext<CR>', opts)
vim.keymap.set('n', '<M-p>', ':cprevious<CR>', opts)
