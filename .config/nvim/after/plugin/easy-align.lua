local opts = { noremap = true, silent = true }

vim.keymap.set('n', 'ga', ':EasyAlign<CR>', opts)
vim.keymap.set('x', 'ga', ':EasyAlign<CR>', opts)
