local opts = { noremap = true , silent = true }

vim.keymap.set('n', '<leader>f', ":Files<CR>", opts)
vim.keymap.set('n', '<leader>h', ":History<CR>", opts)
vim.keymap.set('n', '<leader>d', ":Diagnostics<CR>", opts)
vim.keymap.set('n', 'gs', ":RG<CR>", opts)

vim.keymap.set('n', '<leader>b', ":Buffers<CR>", opts)

vim.cmd [[
"let g:fzf_preview_window = []
autocmd! FileType fzf tnoremap <buffer> <esc> <c-c>
]]
