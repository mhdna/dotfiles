vim.opt_local.makeprg="shellcheck -f gcc '%'"

local bufopts = { noremap = true, silent = true, buffer = bufnr }

vim.keymap.set('n', '<leader>c', ':w<CR>:silent make<CR>:cwindow<CR><C-w><C-k>', bufopts)
vim.keymap.set('n', '<leader>q', ':cwindow<CR>', bufopts)
vim.keymap.set('n', '<leader>S', ':source ~/.config/nvim/ftplugin/sh.lua<CR>', bufopts)
vim.keymap.set('n', '<M-n>', ':cnext<CR>', bufopts)
vim.keymap.set('n', '<M-p>', ':cprevious<CR>', bufopts)
